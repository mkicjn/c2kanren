# c2kanren
**A very small Lisp interpreter powerful enough to host microKanren**

This project was inspired by a number of sources, and it does what it says on the tin:
build from an imperative language (C), to a functional language (Lisp), to a logic programming language (microKanren).

A key element of this approach is strong support for tail-call optimization (TCO) and aggressive garbage collection (GC).

***Note: Recent revisions to the Lisp interpreter and repository as a whole have rendered below information out-of-date - work in progress, will update soon.***

When compiled with `gcc -Os` on a modern Linux OS, the `lisp-small` implementation is just 1.7 KB larger than Hello World (17.6 KB vs. 15.9 KB).
Yet, it remains powerful enough to meaningfully host its own port of microKanren - without fear of exhausting memory or stack space.

The main files:
* `lisp.c` - a simple Lisp interpreter with the optimizations described above
* `rc.lisp` - a "run commands" style script executed by the interpreter automatically; contains important macro definitions
* `ukanren.lisp` - a port of microKanren to that Lisp, including ergonomic macros and support for reification

To see it work, simply clone the repo and run `./ukanren_demo.sh`.
This compiles the interpreters and runs each of the microKanren ports, demonstrating the canonical `appendo` relation both in the cliche manner and using the most general query.
The first of these also displays each input expression before its result for reference.

## The Lisp

The Lisp interpreter here isn't particularly fast, but what's important is that it is small and simple while retaining the optimizations necessary to make deeply-recursive closures usable.
Initially, the goal was to keep it simple enough that it could be ported into even lower level languages, such as my main project language, [paraforth](https://github.com/mkicjn/paraforth).
That goal has not been completely forgotten, but it _has_ taken the backseat to prioritize getting everything to work well in the first place.

The new `lisp-small` interpreter is a practical attempt at returning to those minimalistic roots without compromising microKanren support.
As proof, `ukanren-small.lisp` is a variant of `ukanren.lisp` which has been (barely) modified to run on that version of the interpreter.
Notes have been added to the below information to indicate differences between the two.

Implementation-wise, the interpreter was originally modeled a bit after SectorLISP and tinylisp, combining interesting aspects of the two with new ideas of my own.
However, over time (and especially as a result of working through various design challenges), things have strayed away from either two and gotten more original.

Here's a breakdown of the interpreter's design, in general and relative to tinylisp and SectorLisp:
* Lexerless recursive descent parser with 1 character lookahead - original, but probably similar to either one since it's such an obvious approach
* Symbols interned as Forth-style counted strings - original
* Types distinguished internally by membership in static array space - unlike tinylisp (which uses NaN boxing) or SectorLISP (which uses comparison to a redefined NIL)
  * Non-symbol atoms represented by a list/pair with a sentinel value at the head to take advantage of cell GC - unlike either
    * Note: `lisp-small` changes this by dropping support for non-symbol atoms. Lambdas/fexprs are consed with their environment, like tinylisp.
* Interpreter structured like McCarthy's meta-circular eval - like either SectorLISP or tinylisp (before TCO)
  * TCO implemented via a trampoline while keeping the interpreter structure mostly intact - somewhat like tinylisp (though it's difficult to tell), but much unlike SectorLISP (which lacks TCO)
    * Note: `lisp-small` avoids some complexity by evaluating primitives as special forms in a base case of eval, somewhat like SectorLISP, but retaining TCO
* Copying GC with pointer offsetting for cells - much like SectorLISP (but upgraded to use forwarding pointers and apply to the environment) and much unlike tinylisp (which simply resets a free-pointer at the toplevel)
* Variadicity/argument pasting by dot notation - exactly like tinylisp; don't know about SectorLISP
* Support for macro expansion at read time - unlike tinylisp or SectorLISP, neither of which support a macro expansion phase
* Support for fexprs - exactly like what tinylisp refers to as macros, and naturally unlike SectorLISP, which doesn't support macros at all
  * This project used to also refer to these as "macros", but they have since been renamed and phased out by `ukanren.lisp` and `ukanren-small.lisp` in favor of read-time macros.

Language-wise, it's arguably closer in spirit to Scheme than to say, Common Lisp, for a variety of reasons.
(Hence why .gitattributes overrides the language to Scheme - have to pick something, right?)
However, it does inherit some features both syntactically and semantically from CL, so I use the term "Lisp" simply for generality's sake.
In a nutshell, the implementation here is as if you took a basic Scheme, renamed things to look more like CL, and compromised on the treatment of NIL.

Here's a more intensive breakdown of the language from the programmer's perspective:
* Lisp-1 namespacing (single namespace for both variables and functions)
* Simple `define`s only by default (no `(define (f args) body)`; use `(define f (lambda args body))`)
  * More ergonomic definitions (e.g., `(defun/defmacro (f args) body)` are enabled by macro definitions in `rc.lisp`.
* Variadicity/argument pasting by dot notation, e.g., `(define curry (lambda (f x) (lambda args (f x . args))))`
* Syntactic sugar for `'x -> (quote x)` but no built-in backquote-unquote (this is also enabled by macros in `rc.lisp`)
* The semantics of nil are somewhere between CL and Scheme:
  * Like CL, `()` self-evaluates, `(not ())` is `t`, and `(car/cdr ())` is `()`.
  * Like Scheme, `nil` does not self-evaluate, `()` is the only "false" value, and `()` is not a symbol.
* Primitive names are CL-like, but `null` is dropped in favor of `not` (i.e., a C-like reading where `!ptr` implies `ptr == NULL`)
  * Default names: `t` (for convenience), `()` (or `'()`, incidentally), `atom`, `not`, `eq`
  * Not defined: `#t`, `#f`, `nil`, `atom?`, `null?`, `null`, `eq?`, `else`
* `let` and `let*` work exactly the same as in either CL or Scheme
* Variadic arithmetic functions and `and`/`or` as in either CL or Scheme (note: use `mod` as in CL, not `modulo` as in Scheme)
  * Note: `lisp-small` is purely symbolic and does not support numbers or arithmetic.
* For type-checking, the `type` primitive returns a value (one of `symbol`, `cons`, `lambda`, `macro`, `primitive`, or `()`) which can be compared with `eq`
  * Note: `lisp-small` does not support `type`.
* Fexprs work very similarly to lambdas (and can be closures), e.g.,
  * `((lambda (x) x) (cons a b))` ~> `((lambda (x) x) (eval '(cons a b)))`
  * `((fexpr (x) x) (cons a b))` ~> `(eval ((lambda (x) x) '(cons a b)))`
* Macros are implemented via a hook in the form of the `expand` function, which, if `define`d at the global scope, will be applied to each expression read by the interpreter before evaluation.
  * The version of `expand` provided by `rc.lisp` works by applying rules from `defmacro` repeatedly until failure, then recurses over sub-expressions.

## The Kanren

The uKanren port is patterned mostly after a talk by its creators, and also using the original paper as a reference occasionally.
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
