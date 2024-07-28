/*
 * Unnamed Lisp interpreter (WIP)
 *
 * This Lisp interpreter is not meant to be especially small or fast, but to be as simple as possible while retaining critical optimizations.
 * Namely, aggressive tail-call optimization and garbage collection are needed to allow deeply recursive functions to the extent possible.
 * The end goal is to eventually port uKanren to this implementation.
 *
 * Certain high-level design choices are/were initially inspired by Justine Tunney's SectorLISP and Dr. Robert van Engelen's tinylisp.
 */

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

#ifdef DEBUG
#undef DEBUG
#define DEBUG(stmt) stmt
#else
#define DEBUG(stmt)
#endif


// **************** Top-level definitions ****************

// X macro: Built-in symbols for which an arithmetic primitive will be defined
// TODO: Move arithmetic functions to own file?
#define FOREACH_ARITH_PRIM(X) \
	X("\001+", l_add) \
	X("\001-", l_sub) \
	X("\001*", l_mul) \
	X("\001/", l_div) \
	X("\003mod", l_mod) \
	X("\001=", l_equal) \
	X("\001>", l_gt) \
	X("\001<", l_lt) \
	X("\002>=", l_gte) \
	X("\002<=", l_lte) \
	X("\003max", l_max) \
	X("\003min", l_min)

// X macro: Built-in symbols for which a primitive (arithmetic or otherwise) will be defined
#define FOREACH_PRIM(X) \
	X("\004cons", l_cons) \
	X("\003car", l_car) \
	X("\003cdr", l_cdr) \
	X("\004atom", l_atom) \
	X("\002eq", l_eq) \
	X("\003not", l_not) \
	X("\004eval", l_eval) \
	X("\005quote", l_quote) \
	X("\004cond", l_cond) \
	X("\006lambda", l_lambda) \
	X("\003let", l_let) \
	X("\005macro", l_macro) \
	X("\003and", l_and) \
	X("\002or", l_or) \
	X("\004type", l_type) \
	FOREACH_ARITH_PRIM(X)

// X macro: All built-in symbols (with or without a corresponding primitive)
#define FOREACH_SYMVAR(X) \
	X("\001t", l_t) \
	X("\006define", l_define) \
	X("\006symbol", l_symbol) \
	X("\006number", l_number) \
	X("\011primitive", l_primitive) \
	FOREACH_PRIM(X)

// Declare a compile-time numeric index for each primitive (later used to index prims[] and prim_syms[])
enum l_prim_e {
#define DEFINE_ENUM_VAL(sym, id) id##_e,
	FOREACH_PRIM(DEFINE_ENUM_VAL)
	NUM_PRIMS
};

// Declare a function for each primitive (later pointed to by elements of prims[])
#define DECLARE_FUNC(sym, id) void *id(void *args, void **cont, void **envp);
FOREACH_PRIM(DECLARE_FUNC)

typedef void *(*l_prim_t)(void *args, void **cont, void **envp); // Function pointer type for Lisp primitives

// Declare character pointer variables for each built-in symbol (later pointed to by elements of prim_syms[])
#define DECLARE_SYMVAR(sym, id) char *id##_sym;
FOREACH_SYMVAR(DECLARE_SYMVAR)

// Designated sentinel values (for when a value is needed that cannot be mistaken for an ordinary input or computation)
#define ERROR ((void *)1)       // used for value errors or failed lookups
#define INCOMPLETE ((void *)2)  // used as part of TCO to signal eval to continue
#define FORWARD ((void *)3)     // used as part of GC to signal copy to avoid duplication
#define LAMBDA ((void *)4)      // used to distinguish closures from unevaluated lists
#define MACRO ((void *)5)       // used to distinguish closures from unevaluated lists
#define NUMBER ((void *)6)      // used for implementing numbers (see below)

// How exactly to represent numeric values is difficult, since they must be distinguishable from pointers.
// SectorLISP does not implement numbers, and tinylisp relies on NaN-boxing.
// However, numbers are useful to have, and NaN-boxing is unappealing since it relies on bit fiddling with doubles.

// The approach chosen for this implementation is to represent numbers by consing a sentinel value to them.
// This doubles the space required to store numbers, but is EXTREMELY convenient because it reuses cell GC.
// The added space overhead, while unfortunate, seems forgivable as Lisp is primarily meant for symbolic computation.

// Numeric type to use and associated conversion specifier
typedef long long l_num_t;
#define NUM_FMT "%lld"

// Union for type punning purposes, in case l_num_t isn't an integral type and can't be cast directly to void *
// Note that sizeof(l_num_t) must NOT be greater than sizeof(void *), or there will be problems
union l_num_u {
	l_num_t as_num;
	void *as_ptr;
};

// How to calculate modulo (placed here because it may differ for integers vs. floating point types)
#define MOD(a,b) ((a)%(b))
//#include <math.h>
//#define MOD(a,b) fmod((a),(b))


// **************** Memory regions and region-based type inference ****************

// Space for cons cells in the form of [cell 0 car, cell 0 cdr, cell 1 car, cell 1 cdr, ...]
// (This memory is managed via garbage collection)
void *cells[MAX_CELL_SPACE];
void **next_cell = cells;

// Space for symbols in the form of counted strings (first char is length), stored consecutively
// (This memory is managed via string interning)
char syms[MAX_SYM_SPACE];
char *next_sym = syms;

// Space for pointers to function pointers of primitives (later used by main to populate defines)
// (This memory is completely static)
l_prim_t prims[NUM_PRIMS] = {
#define DEFINE_PRIM_VAL(sym, id) id,
	FOREACH_PRIM(DEFINE_PRIM_VAL)
};

// Space for pointers to symbolic names of primitives (later used by main to populate defines)
// (This memory is completely static)
char *prim_syms[NUM_PRIMS] = {
#define DEFINE_PRIM_NAME(sym, id) sym,
	FOREACH_PRIM(DEFINE_PRIM_NAME)
};

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
	return *CAR(l);
}

static inline void *cdr(void *l)
{
	return *CDR(l);
}

bool atom(void *l)
{
	// A value should be treated as an atom if it is not a cons cell, OR if it represents a lambda, macro, or number
	if (!IN(l, cells))
		return true;
	void *t = *CAR(l);
	return (t == LAMBDA || t == MACRO || t == NUMBER);
}

// Convenience macros
#define list1(a) cons(a, NULL)
#define list2(a, b) cons(a, list1(b))
#define list3(a, b, c) cons(a, list2(b, c))
#define list4(a, b, c, d) cons(a, list3(b, c, d))

#define caar(x) car(car(x))
#define caadr(x) car(car(cdr(x)))
#define cadar(x) car(cdr(car(x)))
#define cadr(x) car(cdr(x))
#define caddr(x) car(cdr(cdr(x)))
#define cadddr(x) car(cdr(cdr(cdr(x))))
#define cdar(x) cdr(car(x))
#define cdadr(x) cdr(car(cdr(x)))

// Value printing
void print(void *x)
{
	if (!x) {
		printf("()");
	} else if (IN(x, cells) && (car(x) == NUMBER)) {
		union l_num_u num = {.as_ptr = cdr(x)};
		printf(NUM_FMT, num.as_num);
	} else if (IN(x, cells) && (car(x) == LAMBDA || car(x) == MACRO)) {
		// For closures, print the type (lambda/macro), args, body, and env
		printf("{");
		print(car(x) == LAMBDA ? l_lambda_sym : l_macro_sym); // type
		printf(": ");
		print(cadr(x)); // args
		printf(" => ");
		print(caddr(x)); // body
		if (cadddr(x)) {
			printf(" | captured: ");
			print(cadddr(x)); // env
		}
		printf("}");
	} else if (IN(x, cells)) {
		// For lists, first print the head
		printf("(");
		print(car(x));
		// Then print successive elements until encountering NIL or atom
		for (x = cdr(x); x && !atom(x); x = cdr(x)) {
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
	} else if (IN(x, prims)) {
		char *s = prim_syms[(void **)x - (void **)prims];
		printf("{primitive: %.*s}", *s, s + 1);
	} else {
		printf("\033[31m{error}\033[m");
	}
}


// **************** Parser ****************

void *read(void);

char peek = '\0';
char next(void)
{
	// Delay char stream by one to allow lookahead
	char c = peek;
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
	for (char *cmp = syms; cmp < s; cmp += *cmp + 1) { // For each symbol (consecutive counted strings)
		if (memcmp(cmp, s, *s + 1) == 0) { // memcmp checks length byte first
			next_sym = s; // i.e., free s
			return cmp;
		}
	}
	return s;
}

void *number(char *s)
{
	// Parse the newest symbol (pointed at by s) as a number
	// (i.e., return a pointer to a number and free down to s if applicable)
	*next_sym = '\0'; // Null terminate for numeric parsing
	union l_num_u num;
	if (sscanf(s+1, NUM_FMT, &num.as_num) > 0) { // (+1 to skip length byte)
		next_sym = s;
		DEBUG(printf("Parsed number %s -> " NUM_FMT "\n", s+1, num.as_num));
		return cons(NUMBER, num.as_ptr);
	}
	return NULL;
}

void *symbol(void)
{
	// Parse a symbol (and intern it)
	char *s = next_sym++;
	while (peek > ' ' && peek != '(' && peek != ')')
		*(next_sym++) = next();
	if (next_sym == s+1) // Disallow empty symbols
		return ERROR;
	*s = next_sym - (s+1); // Store length in first byte
	// If the symbol is a valid number, return that; otherwise intern string
	void *n = number(s);
	return n ? n : intern(s);
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
		return list2(l_quote_sym, read());
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


// **************** Garbage collection ****************

// The inspiration to use copying garbage collection with pointer offsetting comes from SectorLISP - credit to them for that.
// It has been implemented from scratch here with some enhancements, namely, copying the environment and forwarding pointers.
//
// Without these enhancements, SectorLISP's GC strategy will introduce problems when adding TCO.
// This is noted but not explained clearly in the tinylisp article, which tries to cope with a much simpler strategy that is more akin to having no GC at all.
//
// In short, when you start eliminating nested calls to eval, it becomes possible for lists to contain more than one pointer to the same object in the same eval frame.
// Since the object is not from before the pre-eval point like it normally would be, it won't be skipped during copy() and can get deep copied multiple times.
// When this happens, GC will actually increase memory usage and break pointer equality checks that should be expected to succeed.
//
// The solution is to impurely modify each cell after it's copied, such that copy() can know that it was copied and where to if it is encountered again.
//
// Side note: The addition of a define() function that impurely modifies bindings in the same eval frame when possible is also critical to avoid running out of memory.
// Otherwise, recursive functions will blow up memory with unusable bindings, growing the environment list and keeping values alive unnecessarily.

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
#ifndef DISABLE_GC
	DEBUG(clock_t start = clock();)
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
	DEBUG(double ms = (double)(clock() - start) * 1000.0 / CLOCKS_PER_SEC;)
	DEBUG(printf("Cells used: %ld -> %ld (%ld copied, %.3fms)\n", pre_copy - cells, next_cell - cells, copy_size, ms);)
#else
	DEBUG(printf("Cells used: %ld\n", next_cell - cells);)
#endif
}

void *define(void *k, void *v, void *env)
{
#ifndef DISABLE_GC
	// Update an existing binding for k from this eval call to point to v
	for (void *e = env; e > (void *)pre_eval; e = cdr(e)) {
		if (caar(e) == k) {
			*CDR(car(e)) = v;
			return env;
		}
	}
#endif
	// If not possible, make a new binding
	return cons(cons(k, v), env);
}


/* **************** Interpreter (modified for TCO) **************** */

// The implementation of TCO here aims to avoid deviating too heavily from the original interpreter structure
// The basic idea here is to:
// - Add a new eval function that relies on an infinite loop to step through the evaluation
// - Modify the old interpreter functions to return an expression to continue from instead of calling eval, where possible
// The identifiers cont, envp, and INCOMPLETE signal where these modifications happened.

void *defines = NULL; // Global Lisp environment (populated by main at runtime)

void *eval(void *x, void *env);

void *assoc(void *k, void *env)
{
	// Search for value of key k in the current environment
	for (void *bs = env; IN(bs, cells); bs = cdr(bs))
		if (caar(bs) == k) // string interning allows ==
			return cdar(bs);
	// If we failed to find k in local bindings, try again with global bindings (as from define)
	if (env != defines)
		return assoc(k, defines);
	// Otherwise, give up
	printf("\033[31mUnbound variable: ");
	print(k);
	printf("\033[m\n");
	return ERROR;
}

void *evlis(void *l, void *env)
{
	// Map eval over list l (i.e., to form an argument list)
	if (!l)
		return NULL;
	else if (IN(l, cells))
		return cons(eval(car(l), env), evlis(cdr(l), env));
	else
		return eval(l, env); // Append value of dangling atom to list
}

void *pairlis(void *ks, void *vs, void *env)
{
	// Pair keys (ks) with values (vs) in environment env
	for (; IN(ks, cells) && IN(vs, cells); ks = cdr(ks), vs = cdr(vs))
		env = define(car(ks), car(vs), env);
	if (!ks)
		return env;
	return define(ks, vs, env); // Bind remaining values to dangling key
}

void *apply(void *f, void *args, void **cont, void **envp)
{
	// Apply non-primitive function f to unevaluated args in environment env (modified for TCO)
	if (!IN(f, cells))
		return ERROR;

	if (car(f) == MACRO) { // macro -> don't eval args, but do eval result before continuing
		*cont = eval(caddr(f), pairlis(cadr(f), args, cadddr(f)));
		return INCOMPLETE;
	} else if (car(f) == LAMBDA) { // lambda -> eval args, continue with body
		*envp = pairlis(cadr(f), evlis(args, *envp), cadddr(f));
		*cont = caddr(f);
		return INCOMPLETE;
	}
	return ERROR;
}

void *evcon(void *xs, void *env)
{
	// Evaluate cond expressions xs in environment env (modified for TCO)
	for (; IN(xs, cells); xs = cdr(xs))
		if (eval(caar(xs), env))
			return cadar(xs);
	if (!xs)
		return NULL;
	return ERROR;
}

void *evlet(void *ls, void *x, void **envp)
{
       // Evaluate expression x with let bindings ls on top of environment env (modified for TCO)
       for (; IN(ls, cells); ls = cdr(ls))
               *envp = cons(cons(caar(ls), eval(cadar(ls), *envp)), *envp);
       return x;
}

void *eval_step(void **cont, void **envp)
{
	void *x = *cont, *env = *envp;
	// Evaluate expression x in environment env (modified for TCO)
	if (IN(x, syms)) { // Symbol -> return variable binding
		return assoc(x, env);
	} else if (atom(x)) { // Atomic -> return as-is
		return x;
	} else if (IN(x, cells)) { // List -> interpret S-expression
		void *f = eval(car(x), env);
		if (IN(f, prims)) // Primitive -> call C function
			return (*(l_prim_t *)f)(cdr(x), cont, envp);
		else // Non-primitive -> apply lambda or macro
			return apply(f, cdr(x), cont, envp);
	}
	// Unknown -> return as-is
	return x;
}

void *eval(void *x, void *env)
{
	// Tail-call optimized eval
	DEBUG(clock_t start = clock();)
	DEBUG(static int level = 0; level++;)
	DEBUG(printf("%*sL%d eval: ", 4*level, "", level); print(x); printf("\n");)
	void **old_pre_eval = pre_eval;
	pre_eval = next_cell;

	void *ret;
	for (;;) {
		ret = eval_step(&x, &env);
		if (ret != INCOMPLETE)
			break;
		gc(&x, &env); // Collect garbage, keeping the continuation expression

#ifdef DISABLE_TCO
		// If TCO is disabled for testing purposes, eval recursively
		ret = eval(x, env);
		break;
#endif
		DEBUG(printf("%*sL%d step: ", 4*level, "", level); print(x); printf("\n");)
	}
	DEBUG(double ms = (double)(clock() - start) * 1000.0 / CLOCKS_PER_SEC;)
	DEBUG(printf("%*sL%d result: ", 4*level, "", level); print(ret); printf(" (%.3fms)\n", ms);)
	DEBUG(level--;)

	gc(&ret, &env); // Collect garbage, keeping the return value
	pre_eval = old_pre_eval;
	return ret;
}


// **************** REPL ****************

int main()
{
	// Set up symbols and bindings for built-ins using the X macros
#define COPY_SYM(sym, id) \
		id##_sym = next_sym; \
		memcpy(next_sym, sym, sym[0] + 1); \
		next_sym += sym[0] + 1;
	FOREACH_SYMVAR(COPY_SYM)

#define DEFINE_PRIM(sym, id) \
		defines = cons(cons(id##_sym, &prims[id##_e]), defines);
	FOREACH_PRIM(DEFINE_PRIM)

	// Special definitions
	defines = cons(cons(l_t_sym, l_t_sym), defines);

	// Read-eval-print loop
	for (;;) {
		void *nil = NULL;
		void *exp = read();
		if (IN(exp, cells) && car(exp) == l_define_sym) {
			// Handle defines
			if (IN(cadr(exp), syms))
				defines = define(cadr(exp), eval(caddr(exp), NULL), defines);
		} else {
			// Evaluate expressions
			void *res = eval(exp, NULL);
			DEBUG(printf("\033[32m"));
			print(res);
			DEBUG(printf("\033[m"));
			printf("\n");
		}
		gc(&nil, &defines); // Destroy return value and keep global definitions
	}
}


// **************** Primitives ****************

// "Special forms" - i.e., primitives which DO NOT evaluate all their arguments

#define HAS1(args) (IN(args, cells))
#define HAS2(args) (IN(args, cells) && HAS1(cdr(args)))
#define HAS3(args) (IN(args, cells) && HAS2(cdr(args)))
#define REQUIRED(args, n) do { if (!HAS##n(args)) return ERROR; } while (0)

void *l_quote(void *args, void **cont, void **envp)
{
	(void)envp;
	(void)cont; // no TCO
	REQUIRED(args, 1);
	return car(args);
}

void *l_cond(void *args, void **cont, void **envp)
{
	*cont = evcon(args, *envp);
	return INCOMPLETE;
}

void *l_lambda(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	REQUIRED(args, 2);
	return list4(LAMBDA, car(args), cadr(args), *envp);
}

void *l_let(void *args, void **cont, void **envp)
{
	REQUIRED(args, 2);
	*cont = evlet(car(args), cadr(args), envp);
	return INCOMPLETE;
}

void *l_macro(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	REQUIRED(args, 2);
	return list4(MACRO, car(args), cadr(args), *envp);
}

void *l_and(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	void *res = l_t_sym;
	for (; res && IN(args, cells); args = cdr(args))
		res = eval(car(args), *envp);
	return res;
}

void *l_or(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	void *res = NULL;
	for (; !res && IN(args, cells); args = cdr(args))
		res = eval(car(args), *envp);
	return res;
}

// "Functions" - i.e., primitives which DO evaluate all their arguments

void *l_cons(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 2);
	return cons(car(args), cadr(args));
}

void *l_car(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);
	return atom(car(args)) ? ERROR : caar(args);
}

void *l_cdr(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);
	return atom(car(args)) ? ERROR : cdar(args);
}

void *l_atom(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);
	if (atom(car(args)))
		return l_t_sym;
	return NULL;
}

void *l_eq(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 2);
	if (car(args) == cadr(args))
		return l_t_sym;
	if (!IN(car(args), cells) || !IN(cadr(args), cells))
		return NULL;
	if (caar(args) == NUMBER && caadr(args) == NUMBER)
		return cdar(args) == cdadr(args) ? l_t_sym : NULL;
	return NULL;
}

void *l_not(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);
	if (car(args))
		return NULL;
	return l_t_sym;
}

void *l_eval(void *args, void **cont, void **envp)
{
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);
	*cont = car(args);
	return INCOMPLETE;
}

void *l_type(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);

	void *x = car(args);
	if (!x) {
		return NULL;
	} else if (IN(x, syms)) {
		return l_symbol_sym;
	} else if (IN(x, prims)) {
		return l_primitive_sym;
	} else if (IN(x, cells)) {
		void *t = car(x);
		if (t == NUMBER)
			return l_number_sym;
		else if (t == LAMBDA)
			return l_lambda_sym;
		else if (t == MACRO)
			return l_macro_sym;
		else
			return l_cons_sym;
	} else {
		return ERROR;
	}
}

// Arithmetic functions

// These need to be especially careful with the types of their arguments since numbers are cons pairs.
// Other primitives rely on car() and cdr() to return ERROR if something goes wrong, which is normally fine.
// However, if that happens here, it could be misinterpreted as a value, so extra checking is needed.

// TODO: Consider simplifying these functions, even if it means breaking Scheme-like behavior.
// TODO: Consider removing some of these functions, since many aren't needed for the goals of this project.

void *l_add(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args

	// Start with an initial sum of 0
	union l_num_u res = {.as_num = 0};
	// For each argument:
	for (; IN(args, cells); args = cdr(args)) {
		// Ensure the argument is a number
		void *arg = car(args);
		if (car(arg) != NUMBER)
			return ERROR;
		// Add it to the to sum
		union l_num_u n = {.as_ptr = cdr(arg)};
		res.as_num += n.as_num;
	}
	// Construct a new number with the sum
	return cons(NUMBER, res.as_ptr);
}

void *l_sub(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);

	// Grab the first argument
	if (caar(args) != NUMBER)
		return ERROR;
	union l_num_u res = {.as_ptr = cdar(args)};
	// Subtract or negate based on number of arguments
	if (!cdr(args)) { // 1 argument -> negate
		res.as_num = -res.as_num;
	} else { // 2+ arguments -> subtract
		// For each subsequent argument:
		for (args = cdr(args); IN(args, cells); args = cdr(args)) {
			// Ensure the argument is a number
			void *arg = car(args);
			if (car(arg) != NUMBER)
				return ERROR;
			// Subtract it from the result
			union l_num_u n = {.as_ptr = cdr(arg)};
			res.as_num -= n.as_num;
		}
	}
	// Construct a new number with the result
	return cons(NUMBER, res.as_ptr);
}

void *l_mul(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args

	// Start with an initial product of 1
	union l_num_u res = {.as_num = 1};
	// For each argument:
	for (; IN(args, cells); args = cdr(args)) {
		// Ensure the argument is a number
		void *arg = car(args);
		if (car(arg) != NUMBER)
			return ERROR;
		// Multiply it into the product
		union l_num_u n = {.as_ptr = cdr(arg)};
		res.as_num *= n.as_num;
	}
	// Construct a new number with the product
	return cons(NUMBER, res.as_ptr);
}

void *l_div(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args

	// Grab the first argument
	if (caar(args) != NUMBER)
		return ERROR;
	union l_num_u res = {.as_ptr = cdar(args)};
	// Calculate reciprocal or quotient based on number of arguments
	if (!cdr(args)) { // 1 argument -> reciprocal
		res.as_num = (l_num_t)1 / res.as_num;
	} else { // 2+ arguments -> quotient
		// For each subsequent argument:
		for (args = cdr(args); IN(args, cells); args = cdr(args)) {
			// Ensure the argument is a number
			void *arg = car(args);
			if (car(arg) != NUMBER)
				return ERROR;
			// Divide the result by it
			union l_num_u n = {.as_ptr = cdr(arg)};
			res.as_num /= n.as_num;
		}
	}
	// Construct a new number with the result
	return cons(NUMBER, res.as_ptr);
}

void *l_mod(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 2);

	// Grab the first two arguments
	if (caar(args) != NUMBER || caadr(args) != NUMBER)
		return ERROR;
	union l_num_u a = {.as_ptr = cdar(args)};
	union l_num_u b = {.as_ptr = cdadr(args)};
	// Calculate the modulo
	union l_num_u res = {.as_num = MOD(a.as_num, b.as_num)};
	// Construct a new number with the result
	return cons(NUMBER, res.as_ptr);
}

#define COMPARISON_PRIM(name, op) \
void *name(void *args, void **cont, void **envp) \
{ \
	(void)cont; /* no TCO */ \
	args = evlis(args, *envp); /* evaluate args */ \
 \
	/* No arguments -> return true */ \
	if (!IN(args, cells)) \
		return l_t_sym; \
	/* Grab the first argument */ \
	if (caar(args) != NUMBER) \
		return ERROR; \
	union l_num_u cmp = {.as_ptr = cdar(args)}; \
	/* For each subsequent argument: */ \
	for (args = cdr(args); IN(args, cells); args = cdr(args)) { \
		/* Ensure the argument is a number */ \
		void *arg = car(args); \
		if (car(arg) != NUMBER) \
			return ERROR; \
		/* If comparison fails, return false */ \
		union l_num_u n = {.as_ptr = cdr(arg)}; \
		if (!(n.as_num op cmp.as_num)) \
			return NULL; \
		/* Continue comparison from this argument */ \
		cmp.as_num = n.as_num; \
	} \
	/* Ran out of arguments -> return true */ \
	return l_t_sym; \
}

COMPARISON_PRIM(l_equal, ==)
COMPARISON_PRIM(l_gt, >)
COMPARISON_PRIM(l_lt, <)
COMPARISON_PRIM(l_gte, >=)
COMPARISON_PRIM(l_lte, <=)

void *l_max(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);

	// Grab the first argument
	if (caar(args) != NUMBER)
		return ERROR;
	union l_num_u res = {.as_ptr = cdar(args)};
	// For each subsequent argument:
	for (args = cdr(args); IN(args, cells); args = cdr(args)) {
		// Ensure the argument is a number
		void *arg = car(args);
		if (car(arg) != NUMBER)
			return ERROR;
		// Collect the maximum
		union l_num_u n = {.as_ptr = cdr(arg)};
		if (n.as_num > res.as_num)
			res.as_num = n.as_num;
	}
	// Construct a new number with the result
	return cons(NUMBER, res.as_ptr);
}

void *l_min(void *args, void **cont, void **envp)
{
	(void)cont; // no TCO
	args = evlis(args, *envp); // evaluate args
	REQUIRED(args, 1);

	// Grab the first argument
	if (caar(args) != NUMBER)
		return ERROR;
	union l_num_u res = {.as_ptr = cdar(args)};
	// For each subsequent argument:
	for (args = cdr(args); IN(args, cells); args = cdr(args)) {
		// Ensure the argument is a number
		void *arg = car(args);
		if (car(arg) != NUMBER)
			return ERROR;
		// Collect the maximum
		union l_num_u n = {.as_ptr = cdr(arg)};
		if (n.as_num < res.as_num)
			res.as_num = n.as_num;
	}
	// Construct a new number with the result
	return cons(NUMBER, res.as_ptr);
}
