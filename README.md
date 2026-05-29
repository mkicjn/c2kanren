# c2kanren
**A very small hackable Lisp interpreter in C, with its own miniKanren**

This project was inspired by a number of sources, and it does what it says on the tin:
build from an imperative language (C), to a functional language (Lisp), to a relational language (miniKanren).

The `c2klisp` interpreter herein is <500 SLOC, but remains powerful enough to meaningfully host miniKanren.
Central to this ability are its guarantees of tail-call optimization (TCO) and aggressive garbage collection (GC).

The most powerful miniKanren port included here corresponds to the core miniKanren with disequality constraints.

Additionally, a few earlier versions are included as examples to demonstrate the feature set progression from sokuza-kanren,
as well as a version corresponding to μKanren written in a more instructive "literate" style, with unit tests.

The main files:
* `c2klisp.c` - a simple Lisp interpreter with the optimizations described above
* `rc.lisp` - a "run commands" style script executed by the interpreter automatically; contains important macro definitions
* `literate-ukanren.lisp` - a μKanren-like port for that Lisp, written in a literate style with unit tests to introduce streams, unification, and goals in a (hopefully natural) progression
* `sokuza-kanren.lisp` - a port of sokuza-kanren, for simplicity
* `ukanren.lisp` - above, but extended with streams to approximate μKanren
* `minikanren.lisp` - above, but extended with disequality constraints to approximate miniKanren
* `demo.sh` - a script that runs all of the above \*Kanren ports in succession

To see it work, simply clone the repo and run `./demo.sh`.
Requires only `make` and a C compiler. (If `tcc` is available, it will be used with `-run`.)

When `literate-ukanren.lisp` is executed, a sequence of `t`s will indicate unit test successes, followed by several demonstrations.
The other ports only include various demonstrations.


### Lisp Interpreter & Dialect Details

The Lisp interpreter here isn't particularly fast, but what's important is that it is small and simple while retaining the optimizations necessary to make deeply-recursive closures usable.
Initially, the goal was to keep it simple enough that it could be ported into even lower level languages, such as my main project language, [paraforth](https://github.com/mkicjn/paraforth).
That goal has not been completely forgotten, but it _has_ taken the backseat to prioritize getting everything to work well in the first place.

Implementation-wise, the interpreter was originally modeled a bit after SectorLISP and tinylisp, combining interesting aspects of the two with new ideas of my own.
However, over time (and especially as a result of working through various issues), the design has become more original.

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
  * Like CL, `()` self-evaluates to the empty list, `(not ())` is `t`, the `car`/`cdr` of `()` is `()`, and the empty list is a symbol and the only false value.
  * HOWEVER: Like Scheme, the name `nil` is not recognized as a representation of the empty list.
* Primitive names are CL-like, but `null` is dropped in favor of `not` (i.e., a C-like reading where `!ptr` is typically equivalent to `ptr == NULL`)
  * Default names: `t` (for convenience), `()` (or `'()`, incidentally), `atom`, `not`, `eq`
  * Not defined: `#t`, `#f`, `nil`, `atom?`, `null?`, `null`, `eq?`, `else`
* `let` and `let*` work more-or-less the same as in either CL or Scheme
* Variadic `and`/`or` as in either CL or Scheme (note: instead of CL's `mod` or Scheme's `modulo`, use the C-like `%`)
* Macros are implemented via a hook in the form of the `expand` function, which, if `define`d at the global scope, will be applied to each expression read by the interpreter before evaluation.
  * The version of `expand` provided by `rc.lisp` works by applying rules from `defmacro` repeatedly until failure, then recurses over sub-expressions.

Many programming examples are available in the `extras/` directory,
which serves as a dumping ground for other experiments/mini-projects of mine with this interpreter.
