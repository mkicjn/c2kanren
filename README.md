# c2kanren
**A simple Lisp interpreter with full TCO and GC, from scratch, running microKanren**

This is a small project inspired by a number of sources, but it does what it says on the tin.

The main files:
* `lisp.c` - a simple Lisp interpreter with full tail-call optimization and aggressive garbage collection
* `ukanren.lisp` - a port of microKanren to that Lisp, with useful macros and reification

## The Lisp

The Lisp interpreter here isn't that small or fast, but what's important is that it works very simply and has the important optimizations necessary to make deeply-recursive closures usable.
Initially, the goal was to keep it simple enough that it could be ported into even lower level languages, such as my main project language, [paraforth](https://github.com/mkicjn/paraforth).
That goal is still there, but it _has_ taken the backseat slightly to prioritize just getting everything to work well in the first place.
A minimization/simplification pass might be due in the future.
For instance, about 25% of the source code is just for arithmetic primitives _alone_, which is stunning.
If it were easier to do without numbers, I would be keen on deleting those.

Implementation-wise, it was originally modeled a little bit after both SectorLISP and tinylisp, combining interesting aspects of the two with some ideas of my own.
However, over time (and especially as I worked through the challenges of combining TCO and GC), things strayed further and further from either two and got a little more original.

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

Language-wise, it's arguably closer in spirit to Scheme than to say, Common Lisp, for a variety of reasons.
(Hence why .gitattributes overrides the language to Scheme - have to pick something, right?)
However, it does inherit some features both syntactically and semantically from CL, so I use the term "Lisp" simply for generality's sake.
In a nutshell, the implementation here is as if you took a basic Scheme, renamed things to look more like CL, and compromised on the treatment of NIL.

Here's a more intensive breakdown of the language from the programmer's perspective:
* Lisp-1 namespacing (single namespace for both variables and functions)
* Simple `define`s only by default (no `(define (f args) body)`; use `(define f (lambda args body))`)
  * More ergonomic definitions are possible with macro definitions
  * Note: There are no "real" special forms - `cond`, `let`, etc. and even `define` and `lambda` are actually primitives that can be treated as values
* Variadicity/argument pasting by dot notation, e.g., `(define curry (lambda (f x) (lambda args (f x . args))))`
* Syntactic sugar for `'x -> (quote x)` but no built-in backquote-unquote (this is also done with macros)
* The semantics of nil are somewhere between CL and Scheme:
  * Like CL, `()` is the only "false" value and `(not ())` is `t`.
  * Like Scheme, `nil` is not recognized, `()` is not a symbol, and `(car/cdr ())` is an error.
* Primitive names are CL-like, but `null` is dropped in favor of `not` (i.e., a C-like reading where `!ptr` ~= `ptr == NULL`)
  * Default names: `t` (for convenience), `()` (or `'()`, incidentally), `atom`, `not`, `eq`
  * Not defined: `#t`, `#f`, `nil`, `atom?`, `null?`, `null`, `eq?`, `else`
* `let` works exactly the same as a Scheme `let*`
* Variadic arithmetic functions and `and`/`or` as in either CL or Scheme (note: use `mod` as in CL, not `modulo` as in Scheme)
* For type-checking, the `type` primitive returns a value (one of `symbol`, `cons`, `lambda`, `macro`, `primitive`, or `()`) which can be compared with `eq`
* Macros work very similarly to lambdas (and can be closures), e.g.,
  * `((lambda (x) x) (cons a b))` ~> `((lambda (x) x) (eval '(cons a b)))`
  * `((macro (x) x) (cons a b))` ~> `(eval ((lambda (x) x) '(cons a b)))`

## The Kanren

The microKanren port is patterned mostly after a talk by its creators, and also using the original paper as a reference occasionally.
The original work that followed that talk a little more closely is in `ukanren-old.lisp`, and has tons of code commented out where things were being tested and updated.
I figured it might be useful to keep that old body of code around as a reference, but the other two versions are probably much better to read and use.
The code in `ukanren-annotated.lisp` is a cleaned up and _very, very heavily_ commented version of `ukanren-old.lisp` originally produced to help decipher some of the complexity.

Meanwhile, the code in `ukanren.lisp` is the latest iteration, which uses no numeric types in its implementation.
That means no more numbers as variables, and no more threading a counter through with all the substitutions.
Instead, variables are formed by cons pairs to ensure uniqueness, and as such, only the pointer comparison operator `eq` is used to compare them.

This was originally so that the interpreter can be pared down and have math support removed, if desired.
Personally, though, I think it just makes the implementation easier to understand, as it's another moving part removed - one which was initially rather confusing to me, as well.
It also provides some additional flexibility, since numbers are no longer assumed to be variables, and variables can carry arbitrary data with them in their `cdr`.
Logic variables are identified by being a list and having an underscore symbol `_` at the head, but the rest of the list is never inspected.
The usefulness of this is debatable, although it is currently being used to identify variables from `run`(`*`) for when they appear in a reified result.

Summary of current features:
* The usual `==`/`conj`/`disj`/`fresh`/`conde`
  * `conj` and `disj` are variadic (like `conj+` and `disj+` in the paper)
  * `fresh` can take multiple arguments and multiple body expressions (adds a `conj`)
* Support for `run` and `run*`, both with reification
* Inverse-eta-delayed relations with `relation` (works like `lambda`)
* Classic `appendo` example included
* No arithmetic used by the implementation core
