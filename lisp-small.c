#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <time.h>

#ifndef MAX_CELL_SPACE
#define MAX_CELL_SPACE 100000
#endif

#ifndef MAX_SYM_SPACE
#define MAX_SYM_SPACE 100000
#endif

// **************** Top-level definitions ****************

// X macro: Built-in symbols
#define FOREACH_SYMVAR(X) \
	X("\001t", t) \
	X("\004cons", cons) \
	X("\003car", car) \
	X("\003cdr", cdr) \
	X("\004atom", atom) \
	X("\002eq", eq) \
	X("\005quote", quote) \
	X("\004cond", cond) \
	X("\006lambda", lambda) \
	X("\006define", define) \
	X("\004eval", eval) \
	X("\005macro", macro)

// Declare character pointer variables for each built-in symbol
#define DECLARE_SYMVAR(sym, id) char *sym_##id;
FOREACH_SYMVAR(DECLARE_SYMVAR)

// Designated sentinel values (for when a value is needed that cannot be mistaken for an ordinary input or computation)
#define ERROR     ((void *)1)  // used as a generic error value
#define FORWARD   ((void *)2)  // used as part of GC to signal copy to avoid duplication
#define CONTINUE  ((void *)3)  // used as part of TCO to signal eval to continue
#define EMPTY     ((void *)4)  // used to represent the absence of any value (not displayed)

// Values returned on certain errors
#define NOT_BOUND  NULL   // returned when unbound variables are looked up
#define NOT_CONS   NULL   // returned on invalid car/cdr operations
#define EVAL_NIL   NULL   // returned when evaluating the empty list


// **************** Memory regions and region-based type inference ****************

// Space for cons cells in the form of [cell 0 car, cell 0 cdr, cell 1 car, cell 1 cdr, ...]
// (This memory is managed via garbage collection)
void *cells[MAX_CELL_SPACE];
void **next_cell = cells;

// Space for symbols in the form of counted strings (first char is length), stored consecutively
// (This memory is managed via string interning)
char syms[MAX_SYM_SPACE];
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

static inline void *car(void *l)
{
	return IN(l, cells) ? *CAR(l) : NOT_CONS;
}

static inline void *cdr(void *l)
{
	return IN(l, cells) ? *CDR(l) : NOT_CONS;
}

// Convenience macros
#define list1(a) cons(a, NULL)
#define list2(a, b) cons(a, list1(b))

#define caar(x) car(car(x))
#define cadar(x) car(cdr(car(x)))
#define cadr(x) car(cdr(x))
#define caddr(x) car(cdr(cdr(x)))
#define caddar(x) car(cdr(cdr(car(x))))
#define cdar(x) cdr(car(x))

// Value printing
void print(void *x)
{
	if (!x) {
		printf("()");
	} else if (car(car(x)) == sym_lambda || car(car(x)) == sym_macro) {
		// Elide the environment when printing closures
		printf("(");
		print(car(x));
		printf(" ...)");
	} else if (IN(x, cells)) {
		// For lists, first print the head
		printf("(");
		print(car(x));
		// Then print successive elements until encountering NIL or atom
		for (x = cdr(x); IN(x, cells); x = cdr(x)) {
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
		printf("%.*s", *s, s + 1);
	} else if (x == ERROR) {
		printf("\033[31m{error}\033[m");
	} else {
		printf("\033[33m{sentinel: %p}\033[m", x);
	}
}

void display(void *x)
{
	if (x == EMPTY)
		return;
	print(x);
	printf("\n");
}


// **************** Parser ****************

void *read(void);

char *preload = NULL; // String to parse before switching to stdin

char peek = '\0';

char next(void)
{
	// Delay char stream by one to allow lookahead
	char c = peek;
	if (preload && *preload != '\0')
		peek = *(preload++);
	else
		peek = getchar();
	return c;
}

void space(void)
{
	// Skip whitespace
	while (peek <= ' ') {
		if (peek == EOF) // Exit on EOF
			exit(0);
		next();
	}
}

void *body(void)
{
	// Parse a list body (i.e., without parentheses)
	space();
	if (peek == ')') {
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
	int len = *s + 1; // Note that s is a counted string
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
	while (peek > ' ' && peek != '(' && peek != ')')
		*(++next_sym) = next();
	if (next_sym == s) // Disallow empty symbols
		return ERROR;
	*s = next_sym - s;
	next_sym++;
	return intern(s);
}

void *read(void)
{
	// Parse a Lisp expression
	space();
	if (peek == ';') { // Comments
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
	if (car(x) == FORWARD) // No need to copy values that have already been copied
		return cdr(x);
	// Deep copy the value normally
	void *a = copy(car(x), diff);
	void *d = copy(cdr(x), diff);
	void *res = (void **)cons(a, d) - diff;
	// Leave a forward pointer to indicate that the cell has already been copied
	*CAR(x) = FORWARD;
	*CDR(x) = res;
	return res;
}

void gc(void **ret, void **env)
{
	// Copying garbage collection for a return value and the environment
	if (next_cell == pre_eval) // Ellide useless calls
		return;
	// Copy the return value and environment as needed, offsetting cells to match their post-GC position
	void **pre_copy = next_cell;
	ptrdiff_t diff = pre_copy - pre_eval;
	void *post_gc_env = copy(*env, diff);
	void *post_gc_ret = copy(*ret, diff);
	// Move the copied cells into the post-GC position
	ptrdiff_t copy_size = next_cell - pre_copy;
	memcpy(pre_eval, pre_copy, copy_size * sizeof(*pre_copy));
	// Correct next_cell to account for GC
	next_cell = pre_eval + copy_size;
	*env = post_gc_env;
	*ret = post_gc_ret;
}

void *bind(void *k, void *v, void *env)
{
	// Try to update an existing binding for k from this eval call
	for (void *e = env; e > (void *)pre_eval; e = cdr(e)) {
		if (caar(e) == k) {
			*CDR(car(e)) = v;
			return env;
		}
	}
	// If not possible, make a new binding
	return cons(cons(k, v), env);
}


/* **************** Interpreter **************** */

void *eval(void *x, void *env);

void *assoc(void *s, void *env)
{
	if (!env)
		return NOT_BOUND;
	if (caar(env) == s)
		return cdar(env);
	return assoc(s, cdr(env));
}

void *evlis(void *l, void *env)
{
	if (!l)
		return NULL;
	if (IN(l, syms))
		return eval(l, env);
	cons(eval(car(l), env), evlis(cdr(l), env));
}

void *pairlis(void *a, void *b, void *env)
{
	if (!a)
		return env;
	if (IN(a, syms))
		return bind(a, b, env);
	return pairlis(cdr(a), cdr(b), bind(car(a), car(b), env));
}

void *evcon(void *cs, void *env)
{
	if (!cs)
		return NULL;
	if (eval(caar(cs), env))
		return cadar(cs);
	return evcon(cdr(cs), env);
}

void *apply(void *f, void *args, void **env)
{
	if (caar(f) == sym_macro)
		return eval(caddar(f), pairlis(cadar(f), args, cdr(f)));
	if (caar(f) == sym_lambda) {
		*env = pairlis(cadar(f), evlis(args, *env), cdr(f));
		return caddar(f);
	}
	return ERROR;
}

void *eval(void *x, void *env)
{
	void **old_pre_eval = pre_eval;
	pre_eval = next_cell;

	// TCO trampoline
	void *res = CONTINUE;
	while (res == CONTINUE) {
		if (!x) {
			res = EVAL_NIL;
		} else if (x == sym_t) {
			res = x;
		} else if (IN(x, syms)) {
			res = assoc(x, env);
		} else if (!IN(x, cells)) {
			res = x;
		} else if (car(x) == sym_quote) {
			res = cadr(x);
		} else if (car(x) == sym_car) {
			res = car(eval(cadr(x), env));
		} else if (car(x) == sym_cdr) {
			res = cdr(eval(cadr(x), env));
		} else if (car(x) == sym_atom) {
			res = !IN(eval(cadr(x), env), cells) ? sym_t : NULL;
		} else if (car(x) == sym_eq) {
			res = eval(cadr(x), env) == eval(caddr(x), env) ? sym_t : NULL;
		} else if (car(x) == sym_cons) {
			res = cons(eval(cadr(x), env), eval(caddr(x), env));
		} else if (car(x) == sym_lambda || car(x) == sym_macro) {
			res = cons(x, env);
		} else {
			// TCO'd forms
			if (car(x) == sym_eval)
				x = eval(cadr(x), env);
			else if (car(x) == sym_cond)
				x = evcon(x, env);
			else
				x = apply(eval(car(x), env), cdr(x), &env);
			// GC for intermediate eval steps
			gc(&x, &env);
		}
	}

	// GC for result
	gc(&res, &env);
	pre_eval = old_pre_eval;
	return res;
}


// **************** REPL ****************

int main()
{
	// Set up symbols using X macro
#define COPY_SYM(sym, id) \
		sym_##id = next_sym; \
		memcpy(next_sym, sym, sym[0] + 1); \
		next_sym += sym[0] + 1;
	FOREACH_SYMVAR(COPY_SYM)

	preload =
	"(define not (lambda (x) (eq x ())))"
	"(define consp (lambda (x) (not (atom x))))"
	"(define list (lambda args args))"
	"(define curry (lambda (f x) (lambda args (f x . args))))"
	"(define Y ((lambda (g) (g g)) (lambda (y) (lambda (f) (f (lambda args (((y y) f) . args)))))))"
	"(define label (macro (x y) (list 'Y (list 'lambda (list x) y))))"
	"\n";

	void *env = NULL;
	void *nil = NULL;
	for (;;) {
		void *expr = read();
		if (car(expr) == sym_define) {
			env = bind(cadr(expr), eval(caddr(expr), env), env);
		} else {
			display(eval(expr, env));
		}
		gc(&nil, &env);
	}
	return 0;
}
