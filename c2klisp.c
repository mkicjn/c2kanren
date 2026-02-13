//usr/bin/env tcc $CFLAGS -run $0 $@; exit $?
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>

// Compile-time variables
#ifndef MAX_CELL_SPACE
#define MAX_CELL_SPACE 100000
#endif

#ifndef MAX_SYM_SPACE
#define MAX_SYM_SPACE 100000
#endif

#ifdef TRACE
#undef TRACE
#define TRACE(x) x
#define DEBUG
#else
#define TRACE(x)
#endif

#ifdef DEBUG
#undef DEBUG
#define DEBUG(x) x
#else
#define DEBUG(x)
#endif


// **************** Top-level definitions ****************

// X macro: Built-in symbols
#define FOREACH_SYMVAR(X) \
	X("\001t", sym_t) \
	X("\004cons", sym_cons) \
	X("\003car", sym_car) \
	X("\003cdr", sym_cdr) \
	X("\004atom", sym_atom) \
	X("\002eq", sym_eq) \
	X("\005quote", sym_quote) \
	X("\002if", sym_if) \
	X("\006lambda", sym_lambda) \
	X("\006define", sym_define) \
	X("\004eval", sym_eval) \
	X("\006expand", sym_expand) \
	X("\006gensym", sym_gensym) \
	X("\001+", sym_add) \
	X("\001-", sym_sub) \
	X("\001*", sym_mul) \
	X("\001/", sym_div) \
	X("\001%", sym_mod) \
	X("\001>", sym_grt) \
	X("\001=", sym_equ)

// Declare character pointer variables for each built-in symbol
#define DECLARE_SYMVAR(sym, id) char *id;
FOREACH_SYMVAR(DECLARE_SYMVAR)

// Designated sentinel values (for when a value is needed that cannot be mistaken for an ordinary input or computation)
#define ERROR     ((void *)1)  // used as a generic error value
#define FORWARD   ((void *)2)  // used for signaling that a cell has already been copied by garbage collection
#define CONTINUE  ((void *)3)  // used for signaling that an expression should be tail call optimized
#define NUMBER    ((void *)4)  // used for signaling that a cell's cdr has a raw numeric value


// **************** Memory regions and region-based type inference ****************

// Space for cons cells in the form of [cell 0 car, cell 0 cdr, cell 1 car, cell 1 cdr, ...]
// (This memory is managed via garbage collection)
void *cells[MAX_CELL_SPACE] = {0};
void **next_cell = cells;

// Space for symbols in the form of counted strings (first char is length), stored consecutively
// (This memory is managed via string interning)
char syms[MAX_SYM_SPACE] = {0};
char *next_sym = syms;

// Macro for determining whether a pointer lies within a given array
#define IN(X,T) ((uintptr_t)(X) >= (uintptr_t)(T) \
		&& (uintptr_t)(X) < (uintptr_t)(T) + sizeof(T))


// **************** Basic value operations ****************

// List operations
void *cons(void *x, void *y)
{
	void **car = next_cell++;
	void **cdr = next_cell++;
	*car = x;
	*cdr = y;
	return car;
}

#define CAR(l) ((void **)(l))
#define CDR(l) ((void **)(l) + 1)

static inline bool atom(void *l)
{
	return !IN(l, cells) || *CAR(l) == NUMBER;
}

static inline void *car(void *l)
{
	if (!l)
		return NULL;
	return !atom(l) ? *CAR(l) : ERROR;
}

static inline void *cdr(void *l)
{
	if (!l)
		return NULL;
	return !atom(l) ? *CDR(l) : ERROR;
}

// Convenience macros
#define list1(a) cons(a, NULL)
#define list2(a, b) cons(a, list1(b))

#define caar(x) car(car(x))
#define cadar(x) car(cdr(car(x)))
#define caddar(x) car(cdr(cdr(car(x))))
#define cadddr(x) car(cdr(cdr(cdr(x))))
#define caddr(x) car(cdr(cdr(x)))
#define cadr(x) car(cdr(x))
#define cdar(x) cdr(car(x))

// Number-related definitions
#define NUM intptr_t
#define NUM_FMT "%ld"
#define ISNUM(x) (IN((x),cells) && *CAR(x) == NUMBER)
#define UNWRAP(x) (NUM)(*CDR(x))
#define WRAP(x) cons(NUMBER, (void *)(x))

// Value printing
void print(void *x)
{
	if (!x) {
		printf("()");
	} else if (IN(x, cells)) {
		if (*CAR(x) == NUMBER) {
			printf(NUM_FMT, UNWRAP(x));
			return;
		}
		// For lists, first print the head
		printf("(");
		print(car(x));
		// Then print successive elements until encountering NIL or atom
		for (x = cdr(x); !atom(x); x = cdr(x)) {
			printf(" ");
			print(car(x));
		}
		// If encountering an atom, print with dot notation
		if (x) {
			printf(" . ");
			print(x);
		}
		printf(")");
	} else if (IN(x, syms)) {
		char *s = x;
		if (*s == 0)
			printf("_%lu_", s - syms);
		else
			printf("%.*s", *s, s + 1);
	} else if (x == ERROR) {
		printf("\033[31m{error}\033[m");
	} else {
		printf("\033[33m{sentinel: %p}\033[m", x);
	}
}


// **************** Parser ****************

#ifndef RCFILE
#define RCFILE "rc.lisp"
#endif

// Default files: RCFILE followed by stdin
int num_files = 2;
char **files = (char *[]){RCFILE, "-"};

FILE *cur_file = NULL;
void next_file_or_exit(void)
{
	if (cur_file)
		fclose(cur_file);
	do {
		if (num_files <= 0)
			exit(0);
		else if (files[0][0] == '-' && files[0][1] == '\0')
			cur_file = stdin;
		else
			cur_file = fopen(files[0], "r");
		files++;
		num_files--;
	} while (!cur_file);
}

int peek = '\0';
char next(void)
{
	// Delay char stream by one to allow lookahead
	char c = peek;
	peek = fgetc(cur_file);
	if (peek == EOF)
		next_file_or_exit();
	return c;
}

void space(void)
{
	// Skip whitespace
	while (peek <= ' ')
		next();
}

void *read(void);
void *body(void)
{
	// Parse a list body (i.e., without parentheses)
	space();
	if (peek == ';') { // Ignore line comments inside lists
		while (peek != '\n')
			next();
		return body();
	} else if (peek == ')') {
		return NULL;
	} else if (peek == '.') {
		next(); // Discard .
		return read();
	} else {
		void *x = read();
		return cons(x, body());
	}
}

char *intern(char *s)
{
	// Intern the newest symbol, pointed at by s
	// (i.e., return a pointer to a duplicate symbol and free down to s if one exists)
	int len = *(unsigned char *)s + 1; // Note that s is a counted string
	for (char *cmp = syms; cmp < s; cmp += *cmp + 1) { // For each symbol (consecutive counted strings)
		if (memcmp(cmp, s, len) == 0) {
			next_sym = s; // i.e., free s
			return cmp;
		}
	}
	return s;
}

void *symbol(void)
{
	// Parse a symbol (and intern it)
	char *s = next_sym;
	while (peek > ' ' && peek != '(' && peek != ')') {
		if (peek == '\\')
			next();
		*(++next_sym) = next();
	}
	if (next_sym == s) // Disallow empty symbols
		return ERROR;
	*s = next_sym - s;
	*(++next_sym) = '\0';
	NUM n;
	if (sscanf(&s[1], NUM_FMT, &n) > 0) {
		next_sym = s;
		return WRAP(n);
	}
	return intern(s);
}

void *read(void)
{
	// Parse a Lisp expression
	space();
	if (peek == ';') { // Ignore line comments
		while (peek != '\n')
			next();
		return read();
	} else if (peek == '\'') { // Quoted expression
		next();
		space();
		return list2(sym_quote, read());
	} else if (peek == '(') { // List
		next();
		void *list = body();
		space();
		if (peek != ')')
			return ERROR;
		next();
		return list;
	} else { // Everything else
		void *sym = symbol();
		if (sym == ERROR)
			next(); // Skip problem characters
		return sym;
	}
}


/* **************** Garbage collection **************** */

void **pre_eval = cells;

void *copy(void *x, ptrdiff_t diff)
{
	// Copy an object, offsetting all cell pointers
	if (!IN(x, cells) || (void **)x < pre_eval) // No need to copy values below the pre-eval point
		return x;
	if (*CAR(x) == FORWARD) // No need to copy values that have already been copied
		return cdr(x);
	// Deep copy the value normally
	void *a = *CAR(x) == NUMBER ?  NUMBER : copy(car(x), diff);
	void *d = *CAR(x) == NUMBER ? *CDR(x) : copy(cdr(x), diff);
	void *res = (void **)cons(a, d) - diff;
	// Leave a forward pointer to indicate that the cell has already been copied
	*CAR(x) = FORWARD;
	*CDR(x) = res;
	return res;
}

void gc(void **ret, void **env)
{
	// Copying garbage collection for a return value and the environment
	if (next_cell == pre_eval) // Elide useless calls
		return;
	// Copy the return value and environment as needed, offsetting cells to match their post-GC position
	void **pre_copy = next_cell;
	ptrdiff_t diff = pre_copy - pre_eval;
	void *post_gc_env = copy(*env, diff);
	void *post_gc_ret = copy(*ret, diff);
	// Move the copied cells into their final post-GC position
	ptrdiff_t copy_size = next_cell - pre_copy;
	memcpy(pre_eval, pre_copy, copy_size * sizeof(*pre_copy));
	// Adjust next_cell to free up cells not copied by GC
	next_cell = pre_eval + copy_size;
	*env = post_gc_env;
	*ret = post_gc_ret;
	TRACE(printf("Cells used: %ld -> %ld (%ld copied)\n", pre_copy - cells, next_cell - cells, copy_size);)
}

void *set(void *k, void *v, void *env)
{
	// Try to update an existing binding for k from this eval call
	for (void *e = env; e > (void *)pre_eval; e = cdr(e)) {
		if (caar(e) == k) {
			// Important: destructively modify env for GC
			*CDR(car(e)) = v;
			return env;
		}
	}
	// If not possible, make a new binding
	return cons(cons(k, v), env);
}


/* **************** Interpreter **************** */

void *globals = NULL;

void *get(void *s, void *env)
{
	for (void *kvps = env; !atom(kvps); kvps = cdr(kvps))
		if (s == caar(kvps))
			return cdar(kvps);
	if (env != globals)
		return get(s, globals);
	return ERROR;
}

void *map(void *(*f)(void *x, void *env), void *l, void *env)
{
	if (!l)
		return NULL;
	if (atom(l))
		return f(l, env);
	return cons(f(car(l), env), map(f, cdr(l), env));
}

void *pairlis(void *ks, void *vs, void *env)
{
	for (; !atom(ks) && !atom(vs); ks = cdr(ks), vs = cdr(vs))
		env = set(car(ks), car(vs), env);
	if (!ks)
		return env;
	return set(ks, vs, env);
}

void *eval(void *x, void *env);
void *apply(void *f, void *args, void **env)
{
	if (caar(f) == sym_lambda) { // lambda -> continue from body after evaluating and binding args
		*env = pairlis(cadar(f), map(eval, args, *env), cdr(f));
		return caddar(f);
	}
	return ERROR;
}

void *eval_base(void *x, void *env)
{
	// Handle self-evaluating expressions
	if (!x) // ()
		return NULL;
	if (x == sym_t) // t
		return x;
	if (IN(x, syms)) // symbol
		return get(x, env);
	if (atom(x))
		return x;

#define TOBOOL(x) ((x) ? sym_t : NULL)
#define BINOP(sym, op, conv) \
	if (car(x) == sym) { \
		void *a = eval(cadr(x), env); \
		void *b = eval(caddr(x), env); \
		if (ISNUM(a) && ISNUM(b)) \
			return conv(UNWRAP(a) op UNWRAP(b)); \
		return ERROR; \
	}
	BINOP(sym_add,  +, WRAP)
	BINOP(sym_sub,  -, WRAP)
	BINOP(sym_mul,  *, WRAP)
	BINOP(sym_div,  /, WRAP)
	BINOP(sym_mod,  %, WRAP)
	BINOP(sym_grt,  >, TOBOOL)
	BINOP(sym_equ, ==, TOBOOL)

	// Handle primitive functions
	if (car(x) == sym_quote) // quote
		return cadr(x);
	if (car(x) == sym_car) // car
		return car(eval(cadr(x), env));
	if (car(x) == sym_cdr) // cdr
		return cdr(eval(cadr(x), env));
	if (car(x) == sym_atom) // atom
		return atom(eval(cadr(x), env)) ? sym_t : NULL;
	if (car(x) == sym_eq) // eq
		return eval(cadr(x), env) == eval(caddr(x), env) ? sym_t : NULL;
	if (car(x) == sym_cons) // cons
		return cons(eval(cadr(x), env), eval(caddr(x), env));
	if (car(x) == sym_lambda) // lambda
		return cons(x, env);
	if (car(x) == sym_gensym) { // gensym
		next_sym[0] = 0;
		return (next_sym++);
	}

	// Otherwise, not a base case
	return CONTINUE;
}

void *eval(void *x, void *env)
{
	void **old_pre_eval = pre_eval;
	pre_eval = next_cell;
	TRACE(static int level = 0; level++; printf("%*sL%d eval: ", 4*level, "", level); print(x); printf("\n");)

	// Trampoline
	void *ret;
	while ((ret = eval_base(x, env)) == CONTINUE) {
		if (car(x) == sym_eval) // eval -> continue from expression given by evaluated argument
			x = eval(cadr(x), env);
		else if (car(x) == sym_if) // if -> continue from expression switched by condition
			x = eval(cadr(x), env) ? caddr(x) : cadddr(x);
		else // must be a function application -> continue from apply (lambda body / fexpr result)
			x = apply(eval(car(x), env), cdr(x), &env);
		// GC for intermediate eval steps
		gc(&x, &env);
		TRACE(printf("%*sL%d step: ", 4*level, "", level); print(x); printf("\n");)
	}

	// GC for result
	gc(&ret, &env);
	pre_eval = old_pre_eval;
	if (ret == ERROR && x != ERROR) {
		printf("\033[31mError evaluating: `");
		print(x);
		printf("`\033[m\n");
	}
	TRACE(printf("%*sL%d result: ", 4*level, "", level); print(ret); printf("\n"); level--;)
	return ret;
}


// **************** REPL ****************

int main(int argc, char **argv)
{
	// Set up symbols using X macro
#define COPY_SYM(sym, id) \
		id = next_sym; \
		memcpy(next_sym, sym, sym[0] + 1); \
		next_sym += sym[0] + 1;
	FOREACH_SYMVAR(COPY_SYM)

	// Change file set from defaults, if specified
	if (argc > 1) {
		files = argv + 1;
		num_files = argc - 1;
	}
	next_file_or_exit();

	// Run REPL
	void *nil = NULL;
	for (;;) {
		void *expr = read();
		DEBUG(printf("\033[34mRead: "); print(expr); printf("\033[m\n");)
		void *expand = get(sym_expand, globals);
		if (!atom(expand)) {
			expr = eval(list2(sym_expand, list2(sym_quote, expr)), NULL);
			gc(&expr, &globals);
			DEBUG(printf("\033[35mExpanded to: "); print(expr); printf("\033[m\n");)
		}
		if (car(expr) == sym_define) {
			void *res = eval(caddr(expr), NULL);
			globals = set(cadr(expr), res, globals);
			DEBUG(printf("\033[32mDefined "); print(cadr(expr)); printf(" as: "); print(res); printf("\033[m\n");)
		} else {
			void *res = eval(expr, NULL);
			DEBUG(printf("\033[32mEvaluated to: "));
			print(res);
			DEBUG(printf("\033[m"));
			printf("\n");
		}
		gc(&nil, &globals);
		DEBUG(printf("\033[36mCells used: %ld\033[m\n", next_cell - cells);)
	}
	return 0;
}
