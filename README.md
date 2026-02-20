# c2kanren
**A very small hackable Lisp interpreter in C, with its own port of μKanren**

This project was inspired by a number of sources, and it does what it says on the tin:
build from an imperative language (C), to a functional language (Lisp), to a logic programming language (μKanren).

The `c2klisp` interpreter herein is <500 SLOC, but remains powerful enough to meaningfully host its own port of μKanren (also included).
Central to this ability are its guarantees of tail-call optimization (TCO) and aggressive garbage collection (GC).

The main files:
* `c2klisp.c` - a simple Lisp interpreter with the optimizations described above
* `rc.lisp` - a "run commands" style script executed by the interpreter automatically; contains important macro definitions
* `ukanren.lisp` - a port of μKanren to that Lisp, including ergonomic macros and support for reification

To see it work, simply clone the repo and run `./ukanren_demo.sh`.
This compiles the interpreter and runs each of the μKanren ports, demonstrating the canonical `appendo` relation both in the cliche manner and using the most general query.
The first of these also displays each input expression before its result for reference.

## The Lisp

The Lisp interpreter here isn't particularly fast, but what's important is that it is small and simple while retaining the optimizations necessary to make deeply-recursive closures usable.
Initially, the goal was to keep it simple enough that it could be ported into even lower level languages, such as my main project language, [paraforth](https://github.com/mkicjn/paraforth).
That goal has not been completely forgotten, but it _has_ taken the backseat to prioritize getting everything to work well in the first place.

Implementation-wise, the interpreter was originally modeled a bit after SectorLISP and tinylisp, combining interesting aspects of the two with new ideas of my own.
However, over time (and especially as a result of working through various design challenges), things have strayed away from either two and gotten more original.

Here's a breakdown of the interpreter's design, in general and relative to tinylisp and SectorLisp:
* Lexerless recursive descent parser with 1 character lookahead - original, but probably similar to either since it's an obvious approach
* Symbols interned as Forth-style counted strings - original
* Types distinguished internally by membership in static array space - unlike tinylisp (which uses NaN boxing) or SectorLISP (which uses comparison to a redefined NIL)
  * Numbers are represented by a cons pair with a sentinel value at the head to take advantage of cell GC - unlike either
* Interpreter structured like McCarthy's meta-circular eval - like either SectorLISP or tinylisp (before TCO)
  * TCO implemented via a trampoline while keeping the interpreter structure mostly intact - somewhat like tinylisp (though it's difficult to tell), but much unlike SectorLISP (which lacks TCO)
* Copying GC with pointer offsetting for cells - much like SectorLISP (but upgraded to use forwarding pointers and apply to the environment) and much unlike tinylisp (which simply resets a free-pointer at the toplevel)
* Variadicity/argument pasting by dot notation - exactly like tinylisp; don't know about SectorLISP
* Support for macro expansion at read time - unlike tinylisp or SectorLISP, neither of which support a macro expansion phase

Language-wise, it's closer *in spirit* to a subset of Scheme, and closer *in practice* to a subset of Common Lisp.
In a nutshell, it's as if you take a stripped-down Scheme, and changed the names and handling of nil to look a lot more like (but not exactly like) CL.

Here's a more intensive breakdown of the language from the programmer's perspective:
* Lisp-1 namespacing (single namespace for both variables and functions)
* Simple `define`s only *by default* (no `(define (f args) body)`; use `(define f (lambda args body))`)
  * HOWEVER: Ergonomic definitions (e.g., `(defun/defmacro (f args) body)` are enabled by macro definitions in `rc.lisp`.
* Variadicity/argument pasting by dot notation, e.g., `(define curry (lambda (f x) (lambda args (f x . args))))`
* Syntactic sugar for `'x -> (quote x)` but no built-in backquote-unquote (this is also supported by macros in `rc.lisp`)
* The semantics of nil are somewhere between CL and Scheme:
  * Like CL, `()` self-evaluates to the empty list, `(not ())` is `t`, `(car/cdr ())` is `()`, and the empty list is a symbol and the only false value.
  * HOWEVER: Like Scheme, the name `nil` is not recognized as a representation of the empty list.
* Primitive names are CL-like, but `null` is dropped in favor of `not` (i.e., a C-like reading where `!ptr` implies `ptr == NULL`)
  * Default names: `t` (for convenience), `()` (or `'()`, incidentally), `atom`, `not`, `eq`
  * Not defined: `#t`, `#f`, `nil`, `atom?`, `null?`, `null`, `eq?`, `else`
* `let` and `let*` work exactly the same as in either CL or Scheme
* Variadic `and`/`or` as in either CL or Scheme (note: instead of CL's `mod` or Scheme's `modulo`, use the C-like `%`)
* Macros are implemented via a hook in the form of the `expand` function, which, if `define`d at the global scope, will be applied to each expression read by the interpreter before evaluation.
  * The version of `expand` provided by `rc.lisp` works by applying rules from `defmacro` repeatedly until failure, then recurses over sub-expressions.

## The Kanren

The uKanren port is patterned mostly after a talk by its creators, and also using the original paper as a reference occasionally.
The original work that followed that talk a little more closely was in `ukanren-old.lisp` (see the `old-interpreters` branch), and has tons of code commented out where things were being tested and updated.
I figured it might be useful to keep that old body of code around as a reference, but the two versions present now are probably much better to read and use.
The code in `ukanren-annotated.lisp` is a cleaned up and _very, very heavily_ commented version of `ukanren-old.lisp` originally produced to help decipher some of the complexity.

Meanwhile, the code in `ukanren.lisp` is the latest iteration, which uses no numeric types at all in its implementation (except at the very end, just to limit the number of results from `run`).
That means no numbers as variables, and no threading a counter through with all the substitutions.
Instead, variables are formed by cons pairs to ensure uniqueness, and as such, only the pointer comparison operator `eq` is used to compare them.

This was originally so that the interpreter can be pared down and have math support removed, if desired.
(This is very easy to do, and `ukanren.lisp` will essentially still work - try applying `c2kanren-min.diff`.)

Personally, I think it also makes the implementation easier to understand, since it's another moving part removed - one which was initially rather confusing to me, as well.
It also provides some additional flexibility, since numbers are no longer assumed to be variables, and variables can carry arbitrary data with them in their `cdr`.
Logic variables are identified by being a list and having an underscore symbol `_` at the head, but the rest of the list is never inspected.
The usefulness of this is probably debatable, but it is at least used to identify variables from `run`(`*`) for when they appear in a reified result.

Summary of current features:
* The usual `==`/`conj`/`disj`/`fresh`/`conde`
  * `conj` and `disj` are variadic (like `conj+` and `disj+` in the paper)
  * `fresh` can take multiple arguments and multiple body expressions (adds a `conj`)
* Support for `run` and `run*`, both with reification
* Inverse-η-delayed relations with `relation` (works like `lambda`)
* Classic `appendo` example included (demonstrated with `./ukanren_demo.sh`)
* No arithmetic used by the implementation core
