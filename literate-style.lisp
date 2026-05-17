; This source code is a personal attempt at explaining the miniKanren-like languages from scratch,
; so it's written in a "literate style" based on personal understanding and may deviate in terminology.


; First of all, this particular Lisp has some quirks related to runtime type checking,
; so we start with some auxiliary definitions to this effect.

(defun (function? x)
  (cond ((atom x) ())
	((atom (car x)) ())
	((eq (caar x) 'lambda) t)))

(defun (list? x)
  (and (not (atom x)) (not (function? x))))

(defun (number? x)
  (+ x 0))

(defun (symbol? x)
  (and (atom x) (not (number? x))))

; Also, some macros for enabling or disabling tests

;(defmacro (test f r) '(define tests-off t)) ; Disable tests
;(defmacro (test f r) f) ; Enable tests (without checking)
(defmacro (test f r) (` equal , f (quote , r))) ; Enable tests (with checking)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Streams and Stream Manipulation

; At its core, the miniKanren family of languages works by manipulating answer streams.
; We'll get to what an "answer" is later, but for now, let's focus on streams.

; What is a stream?
(defun (streamp x)
  (or
    ; A stream is basically either a list...
    (list? x)
    ; ...or a "generator" function.
    (function? x)))

; We can just check (atom x) to see if the stream is empty.

; For example, here's a stream `zeros` that generates infinitely many zeros:
(define zeros
  (cons 0 (lambda () zeros)))

; Here's a function that returns a generator for any given constant:
(defun (repeat x)
  (lambda ()
    (cons x (repeat x))))

; Here's a function that returns a stream representing a sequence of integers.
(defun (seq from to)
  (if (> to from)
    (cons from (lambda () (seq (+ from 1) to)))
    (list to)))

; So, how do we take a certain number of elements from of a stream?
(defun (take n s)
  (cond
    ; First of all, if we're done taking items, we can return the empty list.
    ((> 1 n) ())
    ; Likewise, if we have an empty stream, there's nothing left to take.
    ((atom s) ())
    ; If we have a generator, we can proceed by calling it.
    ((function? s) (take n (s)))
    ; Otherwise, we can take from the head of the stream and continue with the tail.
    (t (cons (car s) (take (- n 1) (cdr s))))))

(test (take 5 zeros)
      (0 0 0 0 0))
(test (take 4 (repeat 'x))
      (x x x x))
(test (take 3 '(1 2 3 4 5 6 7))
      (1 2 3))
(test (take 5 (seq 100 200))
      (100 101 102 103 104))
(test (take 10 (seq 15 20))
      (15 16 17 18 19 20))

; (Because of the way numbers are handled in this Lisp, "take ()" means "take all.")
(test (take () (seq 55 65))
      (55 56 57 58 59 60 61 62 63 64 65))

; We might also consider a `map` function for streams:
(defun (map-stream s f)
  (cond
    ; If we have an empty stream, there is nothing to do
    ((atom s) ())
    ; If we have a generator, return a generator for the rest of the mapped list.
    ((function? s) (lambda () (map-stream (s) f)))
    ; Otherwise, apply the function to the head and continue with the tail.
    (t (cons (f (car s)) (map-stream (cdr s) f)))))

(test (take () (map-stream (seq 5 10) (lambda (n) (+ n 5))))
      (10 11 12 13 14 15))

; What if we want a more powerful map function that can filter out elements, or add new ones?
; This is easy - we can just swap `cons` for `append`, and make our function return a list.
(defun (adjust-stream s f)
  ; Same as `map-stream`
  (cond ((atom s) ())
	((function? s) (lambda () (adjust-stream (s) f)))
	; But append the results from f, which should return a (possibly empty) list of results.
	(t (append (f (car s)) (adjust-stream (cdr s) f)))))

(test (take () (adjust-stream (seq 5 10) (lambda (n) (list (+ n 5))) (seq 5 10)))
      (10 11 12 13 14 15))
(test (take () (adjust-stream (seq 5 10) (lambda (n) (if (= (% n 2) 0) (list n) ()))))
      (6 8 10))
(test (take () (adjust-stream (seq 5 10) (lambda (n) (list n (* n 2)))))
      (5 10 6 12 7 14 8 16 9 18 10 20))

; OK, but suppose we want to combine multiple streams. How do we do this?
; Let's try building a "higher-order" generator, a stream operator.

; Let's try to make a "concatenate" operator that takes two streams and returns another:
(defun (concatenate s1 s2)
  (cond ((atom s1) s2)
	((function? s1) (lambda () (concatenate (s1) s2)))
	(t (cons (car s1) (concatenate (cdr s1) s2)))))

(test (take 10 (concatenate '(1 2 3 4 5) '(11 12 13 14 15)))
      (1 2 3 4 5 11 12 13 14 15))

; It works!

(test (take 10 (concatenate (seq 100 200) (seq 200 300)))
      (100 101 102 103 104 105 106 107 108 109))

; Hmm... but in the above test case, we might imagine we want to see both streams represented.
; Let's try a small modification to get a new "alternating" stream operator:

(defun (alternate s1 s2)
  (cond ((atom s1) s2)
	((function? s1) (lambda () (alternate s2 (s1))))
	(t (cons (car s1) (alternate (cdr s1) s2)))))

(test (take 10 (alternate (seq 100 200) (seq 200 300)))
      (100 200 101 201 102 202 103 203 104 204))

; Looks promising!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Logic Variables and Unification

; In the beginning, it was explained that miniKanren works by manipulating "answer streams."
; Now we're ready to talk about what those answers are.

; Each "answer" is essentially a set of "satisfying assignments" for logic variables.
; Or, in other words, an environment of variable bindings.

; Logic variables are exactly what they sound like - named placeholders.
; We are trying to answer the question of what values to assign them to so that a proposition becomes true.

; For example, if we have the goal to show "(1 2 _X_ 4) = (1 2 3 4)", we can satisfy it by assigning _X_ := 3.
; This process of making two structures equal is called "unification."

; For lack of a better option, we'll represent variables as a cons pair with '_' at the head.
; The tail can hold any value at all.
(defun (var (x ())) (cons '_ x))
(defun (var? x)
  (if (not (atom x)) (eq (car x) '_)))

; We'll know that two variables are equal when their pointers are equal.
(defun (var= x y) (eq x y))

; Since variables are bound according to some environment, unification may modify its environment.
; It does so until it succeeds, returning a new environment, or fails, returning nothing.

; First, we need to know how to look up logic variables in an environment:
(defun (lookup v e)
  ; If our variable is not a variable (anymore), then we're done.
  (if (not (var? v)) v
    ; Otherwise, treat the environment as an association list and do a lookup.
    (let ((x (assoc v e)))
      ; If we get nothing, give up and return the variable itself.
      ; Otherwise, keep looking, since assignments may be chained.
      (if (not x) v (lookup (cdr x) e)))))

; TODO: Explain and add the "occurs check."

; Now we can produce a unification algorithm:
(defun (unify x y e)
  ; First, try to get actual values for both arguments.
  (let ((x (lookup x e))
	(y (lookup y e)))
    (cond
      ; If the two are equal (via pointer comparison or numeric value), we can stop.
      ((eq x y) e)
      ; If one or the other is a logic variable, we can simply bind it to the other.
      ((var? x) (cons (cons x y) e))
      ((var? y) (cons (cons y x) e))
      ; Otherwise, if either one is an atom, the two cannot possibly have the same value.
      ; If they did, we should have returned true already.
      ((atom x) 'fail)
      ((atom y) 'fail)
      ; Lastly, we have the case where both arguments are lists.
      ; In this case, we try to unify the head first, then the tail.
      (t (let ((e (unify (car x) (car y) e)))
	   (if (eq e 'fail) 'fail
	     (unify (cdr x) (cdr y) e)))))))

(test (let ((X (var 'X))) (unify (` 1 2 , X 4) (` 1 2 3 4) ()))
      (((_ . X) . 3)))
(test (let ((X (var 'X))) (unify (` 1 , X , X 4) (` 1 3 3 4) ()))
      (((_ . X) . 3)))
(test (let ((X (var 'X)) (Y (var 'Y))) (unify (` 1 , X , X 4) (` 1 3 3 , Y) ()))
      (((_ . Y) . 4) ((_ . X) . 3)))
(test (let ((X (var 'X))) (unify (` 1 ,. X) (` 1 2 3 4) ()))
      (((_ . X) 2 3 4)))
(test (let ((X (var 'X))) (unify (` 1 ,. X) (` 1 2 3 4) (` (, X . 3))))
      fail)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 3: Goals as Answer Stream Adjustment Functions

; Finally, we can put it all together to get a very small core logic language.
; Part 1 demonstrated a few general operators for streams of any type.
; Part 2 introduced answers as a set of satisfying assignments.

; Part 3 combines these by introducing "goals", and goal constructors.
; A "goal" (foreshadowed in Part 2) is a function from a candidate answer to a set of newly refined answers.
; The set of new answers may be empty if the goal cannot be satisfied in the context of the candidate answer.

; The most fundamental goal constructor, which is for unification, is called `==`:
(defun (== x y)
  ; Return a function that takes one candidate environment,
  (lambda (e)
    ; tries to unify its arguments in that environment,
    (let ((e (unify x y e)))
      ; and returns a stream of (0 to 1) new environments where x and y are unified.
      (if (eq e 'fail) () (list e)))))

(test (take () (let ((X (var 'X)) (Y (var 'Y)))
		 ((== (` 1 , X 3) (` 1 2 , Y))
		  ())))
      ((((_ . Y) . 3) ((_ . X) . 2))))

; ^ Notice that we can pass our goal an empty environment to let it run unburdened by any existing bindings.
; We could also take a stream of candidate environments and filter it through our goal, as we saw in Part 1.

(test (take () (let ((X (var 'X)) (Y (var 'Y)))
	   (adjust-stream
	     (` ((, X . 4))
		((, X . 5))
		((, X . 6))
		((, X . 7))
		((, X . 8)))
	     (== (` 5 , X 7 8) (` 5 6 , Y 8)))))
      ((((_ . Y) . 7) ((_ . X) . 6))))

; Hopefully from this example, it is clear how goals and answer streams relate to each other.
; Goals are functions that can be used to adjust the answer stream, just as before.

; Now, just as we also showed how to combine two streams in Part 1, we can combine answer streams according to two goals.
; This essentially corresponds to logical disjunction, since each output answer satisfies EITHER goal:
(defun (disj g1 g2)
  ; Construct a goal that...
  (lambda (e)
    ; passes the candidate answer to both goals, then combines their outputs.
    (alternate (g1 e) (g2 e))))

; ^ Quick aside: Either `concatenate` or `alternate` will work to combine the two streams in `disj`.
; Using `concatenate` may result in behavior closer to Prolog-style SLD clause resolution.
; However, `alternate` may be preferable in general, in case the first goal produces tons of results (or repeatedly fails).
; This helps solve the problem of unfair enumeration without strategies like iterative deepening.
; (Don't worry if some of this Prolog-based jargon goes over your head - it is not that important.)

; By applying `adjust-stream`, we can also emulate logical conjunction, where answers must satisfy BOTH goals.

; This produces an answer stream where each answer has passed through BOTH goals:
(defun (conj g1 g2)
  ; Construct a goal that...
  (lambda (e)
    ; passes the candidate answer to one goal, then adjusts its output according to the other.
    (adjust-stream (g1 e) g2)))

(test (take () ((let ((X (var 'X)) (Y (var 'Y)))
		  (conj (disj (== X 1) (== X 2))
			(disj (== Y 3) (== Y 4))))
		'()))
      ((((_ . Y) . 3) ((_ . X) . 1))
       (((_ . Y) . 4) ((_ . X) . 1))
       (((_ . Y) . 3) ((_ . X) . 2))
       (((_ . Y) . 4) ((_ . X) . 2))))

; Remember that we can execute a goal unburdened by passing it an empty environment.
; This will become relevant again in just a moment, when we improve the user interface.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 4: Improving the User Interface

; We have a very small logic programming language more-or-less working now.
; The last few issues are mostly a matter of convenience.

; First issue: Having to manually chain calls to `conj` and `disj`.
; We can define the `cond`-like `conde` macro to make our lives easier.

; First, a helper function to place the operators:
(defun (chain op)
  (lambda (forms)
    ; (a b c d) => (op a (op b (op c d)))
    (fold-right
      (lambda (clause x)
	(if (not x) clause
	  (list op clause x)))
      ()
      forms)))

; Now for `conde` itself:
(defmacro (conde . forms)
  ; (conde (a b) (c d)) => (disj (conj a b) (conj c d))
  (` lambda (s) (, ((chain 'disj) (map (chain 'conj) forms)) s)))
; ^ TODO: Use gensym to make macro hygienic?

(test (expand '(conde (a b c) (d e f)))
      (lambda (s) ((disj (conj a (conj b c)) (conj d (conj e f))) s)))

; Second issue: Having to manually instantiate fresh logic variables with `let` and `var`.
; We can introduce the more compact `fresh` notation:

(defmacro (fresh vars . body)
  ; (fresh (a b) c d) => (let ((a (var)) (b (var))) (conj c d))
  (` let , (map (lambda (v) (` , v (var (quote , v)))) vars)
     , ((chain 'conj) body)))

(test (expand '(fresh (a b c) d))
      ((lambda (a b c) d) (var (quote a)) (var (quote b)) (var (quote c))))

(test (expand '(fresh (a b c) d e))
      ((lambda (a b c) (conj d e)) (var (quote a)) (var (quote b)) (var (quote c))))

; Third issue: Interpreting the stream of variable assignments produced by our programs.
; Usually, we have only a few values we're searching for.
; But reconstructing them might require us to trace through tons of different variable assignments.

; First, let's turn an object from the environment into a real thing we can look at.
; In logic programming, this is called "reification." (Literally from rēs "thing" + faciō "make")
(defun (reifier v)
  (lambda (e)
    (let ((x (lookup v e)))
      (cond ((var? x) x)
	    ((atom x) x)
	    (t (cons ((reifier (car x)) e)
		     ((reifier (cdr x)) e)))))))

(test (fresh (X Y Z) ((reifier (list Z)) (list (cons X 'a) (cons Y 'b) (cons Z (cons X Y)))))
      ((a . b)))

(test (fresh (X Y Z) ((reifier (list Z Z)) (list (cons X 'a) (cons Z (cons X Y)))))
      ((a _ . Y) (a _ . Y)))

; Of course, our logic programs return not just one environment, but a stream of many environments.
; This makes it awkward to call `reifier` directly, since we have to `take` first.
; And before we can `take`, we still have to remember to feed our program that empty environment.

; Both annoyances are remedied simultaneously with a macro called `run`.

(defmacro (run q g)
  (` fresh , (if (atom q) (list q) q)
     (map (reifier , (if (atom q) q (cons 'list q)))
	  (, g '()))))

(test (run () Q (conde ((== Q 5)) ((== Q 6) (== Q 7)) ((== Q 8))))
      (5 8))

(test (run () (Q) (conde ((== Q 5)) ((== Q 6) (== Q 7)) ((== Q 8))))
      ((5) (8)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 5: Programming Examples

; Now for some programming examples:
; TODO: `defrelation`
; TODO: Comments, esp. explaining the delayed evaluation

(defun (appendo X Y Z)
  (lambda (e)
    (lambda ()
      ((fresh
	 (X0 Xs Zs)
	 (conde ((== X ())
		 (== Y Z))
		((== X (cons X0 Xs))
		 (== Z (cons X0 Zs))
		 (appendo Xs Y Zs)))) e))))

(run () (A B) (appendo A B '(a b c d e)))
(run 5 (A B C) (appendo A B C))

(defun (conso A D C) (== C (cons A D)))
(defun (caro C A) (fresh (D) (conso A D C)))
(defun (cdro C D) (fresh (A) (conso A D C)))

(defun (suffix P L)
  (lambda (e)
    (lambda ()
      ((conde ((== P L))
	      ((fresh (L1)
		      (cdro L L1)
		      (suffix P L1)))) e))))

(defun (prefix P L)
  (lambda (e)
    (lambda ()
      ((conde ((== P ()))
	      ((fresh (X R1 R2)
		      (conso X R1 P)
		      (conso X R2 L)
		      (prefix R1 R2)))) e))))

(run () Q (prefix Q '(a b c d)))
(run () Q (suffix Q '(a b c d)))

(defun (proper-listo L)
  (lambda (e)
    (lambda ()
      ((conde ((== L ()))
	      ((fresh (X Xs)
		      (conso X Xs L)
		      (proper-listo Xs)))) e))))

(run 5 Q (proper-listo Q))
(run () Q (proper-listo (cons Q 'x)))

(defun (evalo E R)
  (lambda (env)
    (lambda ()
      ((conde
	 ((== E t) (== R t))
	 ((== E ()) (== R ()))
	 ((== E (` quote , R)))
	 ((fresh (A A` B B`)
		 (== E (` cons , A , B))
		 (== R (cons A` B`))
		 (evalo A A`)
		 (evalo B B`)))
	 ((fresh (X X`)
		 (== E (` car , X))
		 (caro X` R)
		 (evalo X X`)))
	 ((fresh (X X`)
		 (== E (` cdr , X))
		 (cdro X` R)
		 (evalo X X`)))) env))))

(run 5 Q (evalo Q '(a b c)))
