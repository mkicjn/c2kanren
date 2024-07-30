# c2kanren
**Creating a simple Lisp interpreter in C and porting uKanren to it**

This is a small project inspired by a number of sources, but it does what it says on the tin.

The main files:
* `lisp.c` - a simple Lisp interpreter with full tail-call optimization and aggressive garbage collection (basically done)
* `kanren.lisp` - a port of uKanren to that Lisp (basically done), eventually to include useful macros and reification (barely started)

## The Lisp

The Lisp interpreter here isn't that small or fast, but what's important is that it works very simply and has the important optimizations necessary to make deeply-recursive closures usable.
Initially, the goal was to keep it simple enough that it could be ported into even lower level languages, such as my main project language, [paraforth](https://github.com/mkicjn/paraforth).
That goal is still there, but it _has_ taken the backseat slightly to prioritize just getting everything to work well in the first place.
A minimization/simplification pass might be due in the future.
For instance, about 25% of the source code is just for arithmetic primitives _alone_, which is stunning.
If it were easier to do without numbers, I would be keen on deleting those.

It also might be an interesting testbed for playing with other language modifications or implementation techniques, such as:
* Linear types (a la Flea scheme)
* Mark-sweep collection with object pools (to avoid copying)

Implementation-wise, it was originally modeled a little bit after both SectorLISP and tinylisp, combining interesting aspects of the two with some ideas of my own.
However, over time (and especially as I worked through the challenges of combining TCO and GC), things strayed further and further from either of the two and got a little more original.

Here's a breakdown of this implementation's design, in general and relative to the other two:
* Lexerless recursive descent parser with 1 character lookahead - original, but probably similar to either one as it is an obvious approach
* Symbols interned as Forth-style counted strings - original
* Types distinguished internally by membership in static array space - unlike tinylisp (which uses NaN boxing) or SectorLISP (which uses comparison to a redefined NIL)
  * Non-symbol atoms represented by a list/pair with a sentinel value to take advantage of GC - unlike either (?)
* Interpreter structured like McCarthy's meta-circular eval - like either SectorLISP or tinylisp (before TCO)
  * TCO implemented with meta-circular eval structure mostly intact - unlike either tinylisp (which folds a lot of code into eval) or SectorLISP (which lacks TCO)
* Copying GC with pointer offsetting for cells - much like SectorLISP (but upgraded to use forwarding pointers and apply to the environment) and much unlike tinylisp (which simply resets a free-pointer at the toplevel)
* Special forms implemented as primitive functions - much like tinylisp (but without the array of structs) and much unlike SectorLISP (which seems to do the reverse by implementing all primitives as special forms)
* Variadicity/argument pasting by dot notation - exactly like tinylisp; don't know about SectorLISP
* Macros work like lambdas - exactly like tinylisp; can't remember if this similarity was intentional

Language-wise, one might argue that it's closer in spirit to a Scheme than to say, Common Lisp, for a variety of reasons.
But really, it inherits features both syntactically and semantically from either one - admittedly, with little thought - so I use the term "Lisp" simply for generality's sake.
In a nutshell, it's like if you took a basic Scheme, renamed everything to make it look more like CL, and compromised on the treatment of NIL.

Here's a more intensive breakdown of the language from the programmer's perspective:
* Lisp-1 namespacing (single namespace for both variables and functions)
* Simple `define`s only (no `(define (f args) body)`; use `(define f (lambda args body))`)
  * Note: `define` is the only "real" special form as `cond`, `let`, etc. and even `lambda` are actually primitives that can be treated as values
* Variadicity/argument pasting by dot notation, e.g., `(define curry (lambda (f x) (lambda args (f x . args))))`
* Syntactic sugar for `'x -> (quote x)` but no backquote-unquote (yet?)
* The semantics of nil are somewhere between CL and Scheme:
  * Like CL, `()` is the only "false" value, and `(not ())` is `t`.
  * Like Scheme, `nil` is not a special symbol, and `(car/cdr ())` is an error.
* Primitive names are CL-like, but `null` is dropped in favor of `not` (i.e., a C-like reading where `!ptr` ~= `ptr == NULL`)
  * Default names: `t` (for convenience), `()` (or `'()`, incidentally), `atom`, `not`, `eq`
  * Not defined: `#t`, `#f`, `nil`, `atom?`, `null?`, `null`, `eq?`, `else`
* `let` works exactly the same as a Scheme `let*`
* Variadic arithmetic functions and `and`/`or` as in either CL or Scheme (note: use `mod` as in CL, not `modulo` as in Scheme)
* Type-checking: `type` returns a symbol (one of `symbol`, `cons`, `lambda`, `macro`, `primitive`, or `()`) which can be compared with `eq`
* Macros work very similarly to lambdas (and can be closures), e.g.,
  * `((lambda (x) x) (cons a b))` ~> `((lambda (x) x) (eval '(cons a b)))`
  * `((macro (x) x) (cons a b))` ~> `(eval ((lambda (x) x) '(cons a b)))`

## The Kanren

The uKanren port is really still in its infancy (much more of a WIP) and is patterned mostly after a talk by the creators of uKanren, linked in the source code comments.
I have some original ideas about an alternative implementation strategy that I want to pursue at some point, but it's too early for that right now. (Note to self: walk -> lazily-evaluated bindings?)
