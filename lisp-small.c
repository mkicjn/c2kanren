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

// X macro: Built-in symbols
#define FOREACH_SYMVAR(X) \
	X("\001t", t) \
	X("\004cons", cons) \
	X("\003car", car) \
	X("\003cdr", cdr) \
	X("\004atom", atom) \
	X("\002eq", eq) \
	X("\003not", not) \
	X("\005quote", quote) \
	X("\004cond", cond) \
	X("\006lambda", lambda) \
	X("\004eval", eval)

// Declare character pointer variables for each built-in symbol
#define DECLARE_SYMVAR(sym, id) char *sym_##id;
FOREACH_SYMVAR(DECLARE_SYMVAR)

// Designated sentinel values (for when a value is needed that cannot be mistaken for an ordinary input or computation)
#define ERROR ((void *)1)       // used for value errors or failed lookups
#define CONTINUE ((void *)2)    // used as part of TCO to signal eval to continue
#define FORWARD ((void *)3)     // used as part of GC to signal copy to avoid duplication


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
	return IN(l, cells) ? *CAR(l) : NULL;
}

static inline void *cdr(void *l)
{
	return IN(l, cells) ? *CDR(l) : NULL;
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


/* **************** Interpreter **************** */

void *eval(void *x, void *env);

void *assoc(void *s, void *env)
{
	if (!env)
		return ERROR;
	if (caar(env) == s)
		return cdar(env);
	return assoc(s, cdr(env));
}

void *evlis(void *l, void *env)
{
	if (!l)
		return NULL;
	cons(eval(car(l), env), evlis(cdr(l), env));
}

void *pairlis(void *a, void *b, void *env)
{
	if (!a || !b)
		return env;
	if (!IN(a, cells))
		return cons(cons(a, b), env);
	return pairlis(cdr(a), cdr(b), cons(cons(car(a), car(b)), env));
}

void *evcon(void *cs, void *env)
{
	if (!cs)
		return NULL;
	if (eval(caar(cs), env))
		return eval(cadar(cs), env);
	return evcon(cdr(cs), env);
}

void *eval(void *x, void *env)
{
	if (!IN(x, cells)) {
		// Non-list expressions
		if (!x)
			return NULL;
		else if (IN(x, syms))
			return assoc(x, env);
		else
			return x;
	} else if (!IN(car(x), cells)) {
		// Primitive operators
		if (car(x) == sym_quote)
			return cadr(x);
		else if (car(x) == sym_car)
			return car(eval(cadr(x), env));
		else if (car(x) == sym_cdr)
			return cdr(eval(cadr(x), env));
		else if (car(x) == sym_atom)
			return IN(eval(cadr(x), env), cells) ? sym_t : NULL;
		else if (car(x) == sym_eq)
			return eval(cadr(x), env) == eval(caddr(x), env) ? sym_t : NULL;
		else if (car(x) == sym_cons)
			return cons(eval(cadr(x), env), eval(caddr(x), env));
		else if (car(x) == sym_lambda)
			return cons(x, env);
		else if (car(x) == sym_cond)
			return evcon(x, env);
	}

	// Complex expressions
	void *f = eval(car(x), env);
	if (!IN(f, cells)) // f => atom
		return eval(cons(f, cdr(x)), env);
	else if (caar(f) == sym_lambda)  // f => ((lambda cadar caddar) . cdr)
		return eval(caddar(f), pairlis(cadar(f), evlis(cdr(x), env), cdr(f)));
	return ERROR;
}


// **************** REPL ****************

int main()
{
	// Set up symbols using the X macro
#define COPY_SYM(sym, id) \
		sym_##id = next_sym; \
		memcpy(next_sym, sym, sym[0] + 1); \
		next_sym += sym[0] + 1;
	FOREACH_SYMVAR(COPY_SYM)

	for (;;) {
		print(eval(read(), NULL));
		printf("\n");
	}
	return 0;
}
