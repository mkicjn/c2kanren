# c2kanren
**Creating a simple Lisp interpreter in C and porting uKanren to it**

This is a small project inspired by a number of sources, but it does what it says on the tin.

The main files:
* `lisp.c` - a simple Lisp interpreter with full tail-call optimization and aggressive garbage collection (basically done)
* `kanren.lisp` - a port of uKanren to that Lisp (basically done), eventually to include useful macros and reification (not started)

## The Lisp

The Lisp interpreter here isn't that small or fast, but what's important is that it works very simply and has the important optimizations necessary to make deeply-recursive closures usable.
Initially, the goal was to keep it simple enough that it could be ported into even lower level languages, such as my main project language, [paraforth](https://github.com/mkicjn/paraforth).
That goal is still there, but it _has_ taken the backseat slightly to prioritize just getting everything to work well in the first place.
A minimization/simplification pass might be due in the future.
For instance, about 25% of the source code is just for arithmetic primitives _alone_, which is stunning.
If it were easier to do without numbers, I would be keen on deleting those.

Implementation-wise, it was originally modeled a little bit after both SectorLISP and tinylisp, combining interesting aspects of the two with some ideas of my own.
However, over time (and especially as I worked through the challenges of combining TCO and GC), things strayed further and further from either of the two and got a little more original.

A brief breakdown of this implementation's design, in general and relative to the other two:
* Lexerless recursive descent parser with 1 character lookahead - original, but probably similar to either one as it is an obvious approach
* Symbols interned as Forth-style counted strings - original
* Types distinguished internally by membership in static array space - unlike tinylisp (which uses NaN boxing) or SectorLISP (which uses comparison to a redefined NIL)
  * Non-symbol atoms represented by a list/pair with a sentinel value to take advantage of GC - unlike either (?)
* Interpreter structured like McCarthy's meta-circular eval - like either SectorLISP or tinylisp (before TCO)
  * TCO implemented with meta-circular eval structure mostly intact - unlike either tinylisp (which folds a lot of code into eval) or SectorLISP (which lacks TCO)
* Copying GC with pointer offsetting for cells - much like SectorLISP (but upgraded to use forwarding pointers and apply to the environment) and much unlike tinylisp (which simply resets a free-pointer at the toplevel)
* Special forms implemented as primitive functions - much like tinylisp (but without the array of structs) and much unlike SectorLISP (which seems to do the reverse by implementing all primitives as special forms)
* Variadicity/argument pasting by dot notation - exactly like tinylisp; don't know about SectorLISP

Language-wise, it's really closer to a Scheme than say, Common Lisp, because of its small size and Lisp-1 namespacing (variables and functions in same namespace).
However, it's not a "standard" Scheme, and there is a bit of a mix between the two stylistically, so I use the term "Lisp" simply for generality's sake.

A brief breakdown of the language from the programmer's perspective:
* Lisp-1 namespacing (variables and functions in same namespace)
* Simple `define`s only (no `(define (f args) body)`; use `(define f (lambda args body))`)
  * Note: `define` is the only "real" special form as `cond`, `let`, etc. are implemented as primitives
* Variadicity/argument pasting by dot notation, e.g., `(define curry (lambda (f x) (lambda args (f x . args))))`
* Syntactic sugar for `'x -> (quote x)` but no backquote-unquote (yet?)
* Default names are generally CL-like, minus `nil` and `null`
  * Included: `t` (for convenience), `()` (or `'()`, incidentally), `atom`, `not`, `eq`
  * Not included: `#t`, `#f`, `nil`, `atom?`, `null?`, `null`, `eq?`
* `let` works exactly the same as a Scheme `letrec`
* Variadic arithmetic functions and `and`/`or` as in either CL or Scheme (note: use `mod` as in CL, not `modulo` as in Scheme)
* Type-checking: `type` returns a symbol (one of `symbol`, `cons`, `lambda`, `macro`, `primitive`) which can be compared with `eq`
* Macros work very similarly to lambdas (and can be closures), e.g.,
  * `((lambda (x) x) (cons a b))` ~> `((lambda (x) x) (eval '(cons a b)))`
  * `((macro (x) x) (cons a b))` ~> `(eval ((lambda (x) x) '(cons a b)))`

## The Kanren

The uKanren port is really still in its infancy (much more of a WIP) and is patterned mostly after a talk by the creators of uKanren, linked in the source code comments.
I have some original ideas about an alternative implementation strategy that I want to pursue at some point, but it's too early for that right now. (Note to self: walk -> lazily-evaluated bindings?)
