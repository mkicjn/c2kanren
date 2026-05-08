; (Heavily WIP)

; This source code is meant to be a personal attempt at implementing and explaining μKanren from scratch,
; so it's written in a "literate style" based on personal understanding and deviates from the paper slightly.

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

; At its core, μKanren (and miniKanren) work by manipulating streams.

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

; So, how do we take an item out of a stream?
; First, we want the stream to be a list so we have something tangible to take.
(defun (advance s)
  (cond
    ; If the stream is empty, there's nothing to take.
    ((atom s) ())
    ; If we encounter a generator function, we'll try calling it.
    ((function? s) (advance (s)))
    ; Otherwise, we can already take one out in the obvious way.
    (t s)))

; Now we can take some numbers from these examples.
(defun (take n s)
  (let ((s (advance s)))
    (cond
      ; If the stream is an atom (probably nil), there's nothing left to take.
      ((atom s) s)
      ; If a number greater than zero was given, count down from there.
      ((> n 0) (if (atom s) () (cons (car s) (take (- n 1) (cdr s)))))
      ; If nil was given, take as many as possible.
      ((not n) (if (atom s) () (cons (car s) (take () (cdr s))))))))

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
(test (take () (seq 55 65))
      (55 56 57 58 59 60 61 62 63 64 65))

; We might also consider a `map` function for streams:
(defun (map-stream s f)
  (let ((s (advance s)))
    (if (atom s) ()
      (cons (f (car s)) (lambda () (map-stream (cdr s) f))))))

; ^ IMPORTANT: Take note of the idiom above; it will be repeated many times!
; Whenever we take a stream as an argument, we generally want to do three things:
; 1. Try to advance the stream.
; 2. Check if the advanced stream is empty.
; 3. Return one concrete result and a generator for the rest.
;
; Why one result? To keep make sure we produce concrete values instead of delaying.
; When we start manipulating streams in more complex ways later, this will help
; us avoid getting stuck in infinite loops building infinitely complex generators.
;
; Why a generator for the rest? In case there are infinitely many results!
; If we don't calculate the values lazily, we risk immediately running out of memory.
; Doing this also makes certain techniques behave more predictably later.

(test (take () (map-stream (seq 5 10) (lambda (n) (+ n 5))))
      (10 11 12 13 14 15))

; What if we want a more powerful map function so we can filter out elements, or add new ones?
; This is easy - we can just swap `cons` for `append`, and make our function return a list.
(defun (adjust-stream s f)
  (let ((s (advance s)))
    (if (atom s) ()
      (append (f (car s)) (lambda () (adjust-stream (cdr s) f))))))

(test (take () (adjust-stream (seq 5 10) (lambda (n) (list (+ n 5))) (seq 5 10)))
      (10 11 12 13 14 15))
(test (take () (adjust-stream (seq 5 10) (lambda (n) (if (= (% n 2) 0) (list n) ()))))
      (6 8 10))
(test (take () (adjust-stream (seq 5 10) (lambda (n) (list n (* n 2)))))
      (5 10 6 12 7 14 8 16 9 18 10 20))

; OK, but suppose we want to combine multiple streams. How do we do this?
; Let's try building a "higher-order" generator, a stream operator.

; Let's try to make a "concatenate" operator that takes two streams and returns another:
(defun (cat s1 s2)
  ; First, advance stream 1.
  (let ((s1 (advance s1)))
    ; If it turns out to be empty, continue from stream 2.
    (if (atom s1) (advance s2)
      ; Otherwise, present the first item from stream 1 and repeat.
      (cons (car s1) (lambda () (cat (cdr s1) s2))))))

(test (take 10 (cat '(1 2 3 4 5) '(11 12 13 14 15)))
      (1 2 3 4 5 11 12 13 14 15))

; It works!

(test (take 10 (cat (seq 100 200) (seq 200 300)))
      (100 101 102 103 104 105 106 107 108 109))

; Hmm... but in the above test case, we might imagine we want to see both streams represented.
; Let's try a small modification to get a new "alternating" stream operator:

(defun (alt s1 s2)
  ; Same as cat...
  (let ((s1 (advance s1)))
    (if (atom s1) (advance s2)
      ; ...EXCEPT: let stream 2 go next before continuing with stream 1.
      (cons (car s1) (lambda () (alt s2 (cdr s1)))))))
;                               ^^^^^^^^^^^^^^^^^

(test (take 10 (alt (seq 100 200) (seq 200 300)))
      (100 200 101 201 102 202 103 203 104 204))

; Looks promising!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Logic Variables and Unification

; Now for a slight detour - what exactly is a logic variable?

; Logic variables basically act like placeholders or "don't cares" in a list.

; For lack of a better option, we'll let logic variables be represented by a pair with '_' at the head.
; (The tail can hold any value at all.)
(defun (var (x '_)) (cons '_ x))
(defun (var? x)
  (if (not (atom x)) (eq (car x) '_)))

; We'll know that two variables are equal when their pointers are equal.
(defun (var= x y) (eq x y))

; The purpose of these is to check if two nested lists can be made equal by assigning variables inside them.
; This process is called "unification."

; For example, (1 2 (_ . X) 4) unified with (1 2 3 4) will assign variable (_ . X) to the value 3.

; Since variables are bound according to some environment, unification modifies its environment.
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
      ; If the two are definitionally equal (via pointer comparison or numeric value), we can stop.
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
; Part 3: Goals as Streams of Unification Results

; Part 1 mentions that the miniKanren family of logic languages works by manipulating streams.
; But, streams of what? Streams of environments from Part 2, of course!

; Part 1 also demonstrated a few general operators for streams.
; Part 3 introduces "goals," which use these to introduce new operators for streams of environments.

; The most fundamental goal constructor in this contex is `==`.
; This goal attempts to unify its arguments in each environment:
(defun (== x y)
  ; Return a unary stream operator that...
  (lambda (s)
    ; ...for each environment in the stream...
    (adjust-stream s
      ; ...tries to unify its arguments in that environment.
      (lambda (e)
	(let ((e (unify x y e)))
	  (if (eq e 'fail) () (list e)))))))

(test (take () (let ((X (var 'X)) (Y (var 'Y)))
		 ((== (` 1 , X 3) (` 1 2 , Y))
		  (` ()))))
      ((((_ . Y) . 3) ((_ . X) . 2))))

; ^ Notice that we pass our goal a stream containing one empty environment to run it in the "general" case.
; We could also manually specify a stream of candidate environments to try:

(test (take () (let ((X (var 'X)) (Y (var 'Y)))
	   ((== (` 5 , X 7 8) (` 5 6 , Y 8))
	    (` ((, X . 4))
	       ((, X . 5))
	       ((, X . 6))
	       ((, X . 7))
	       ((, X . 8))
	       ))))
      ((((_ . Y) . 7) ((_ . X) . 6))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 4: Logic Programs as Combinations of Goals

; But one goal is still not much of a logic program.

; Our goals take a stream of environments and produce a new stream of environments.
; So, it should be possible to join them together to make more interesting programs.

; Goals and environments can be arbitrarily complex in principle.
; TODO: Leverage this flexibility to try to implement constraint logic programming.

; But to start with, we can at least implement two simple logical connectives:
; * Conjunction (logical AND)
; * Disjunction (logical OR)

; Thankfully, these are intuitive with what we have.

; Conjunction: Pass the stream through one goal, then the other.
; This requires each environment in the output stream to pass through BOTH goals.
(defun (conj g1 g2)
  ; Take a stream...
  (lambda (s)
    (let ((s (advance s)))
      (if (atom s) ()
	; ...and pass its contents through both goals.
	(g2 (g1 s))))))

; TODO: Attempt at fair conjunction?

; Disjunction: Pass the stream through both goals and combine the results.
; This requires each environment in the output stream to pass through EITHER goal.
(defun (disj g1 g2)
  ; Take a stream...
  (lambda (s)
    (let ((s (advance s)))
      (if (atom s) ()
	; ...pass it to both goals, and combine them.
	(alt (g1 s) (g2 s))))))

; ^ Either `cat` or `alt` will work to combine the two streams in `disj`.
; Using `cat` may result in behavior closer to Prolog-style SLD clause resolution.
; However, `alt` may be preferable in general, in case the first goal produces tons of results.
; This helps solve the problem of unfair enumeration without strategies like iterative deepening.
; (Don't worry if this Prolog jargon goes over your head - it is not that important!)

(test (take () ((let ((X (var 'X)) (Y (var 'Y)))
		  (conj (disj (== X 1) (== X 2))
			(disj (== Y 3) (== Y 4))))
		'(())))
      ((((_ . Y) . 3) ((_ . X) . 1))
       (((_ . Y) . 4) ((_ . X) . 1))
       (((_ . Y) . 3) ((_ . X) . 2))
       (((_ . Y) . 4) ((_ . X) . 2))))

; Remember that we can run our logic program by passing it a stream containing an empty environment.
; This will become relevant again in just a moment...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 5: Improving the User Interface

; We have a very small logic programming language working now.
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
  ((chain 'disj) (map (chain 'conj) forms)))

(test (expand '(conde (a b c) (d e f)))
      (disj (conj a (conj b c)) (conj d (conj e f))))

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
(defun (reify v e)
  (let ((x (lookup v e)))
    (cond ((var? x) x)
	  ((atom x) x)
	  (t (cons (reify (car x) e)
		   (reify (cdr x) e))))))

(test (fresh (X Y Z) (reify (list Z) (list (cons X 'a) (cons Y 'b) (cons Z (cons X Y)))))
      ((a . b)))

(test (fresh (X Y Z) (reify (list Z Z) (list (cons X 'a) (cons Z (cons X Y)))))
      ((a _ . Y) (a _ . Y)))

; Of course, our logic programs return not just one environment, but a stream of many environments.
; This makes it awkward to call `reify` directly, since we have to `take` first.
; And before we can `take`, we still have to remember to feed our program that empty environment.

; Both annoyances are remedied simultaneously with a macro called `run`.

(defun (reify-all x s)
  (map (lambda (e) (reify x e)) s))

(defmacro (run n q g)
  (` fresh , (if (atom q) (list q) q)
     (reify-all , (if (atom q) q (cons 'list q))
		(take , n (, g '(()))))))

(test (run () Q (conde ((== Q 5)) ((== Q 6) (== Q 7)) ((== Q 8))))
      (5 8))

(test (run () (Q) (conde ((== Q 5)) ((== Q 6) (== Q 7)) ((== Q 8))))
      ((5) (8)))


; Now for some programming examples:
; TODO: `defrelation`

(defun (appendo X Y Z)
  (lambda (s)
    (lambda ()
      ((fresh
	 (X0 Xs Zs)
	 (conde ((== X ())
		 (== Y Z))
		((== X (cons X0 Xs))
		 (== Z (cons X0 Zs))
		 (appendo Xs Y Zs))))
       s))))

(run () (A B) (appendo A B '(a b c d e)))
(run 5 (A B C) (appendo A B C))

(defun (conso A D C)
  (lambda (s) (lambda () ((== C (cons A D)) s))))

(defun (caro C A)
  (lambda (s) (lambda () ((fresh (D) (conso A D C)) s))))

(defun (cdro C D)
  (lambda (s) (lambda () ((fresh (A) (conso A D C)) s))))

(defun (suffix P L)
  (lambda (s)
    (lambda ()
      ((conde ((== P L))
	      ((fresh (L1)
		      (cdro L L1)
		      (suffix P L1))))
       s))))

(defun (prefix P L)
  (lambda (s)
    (lambda ()
      ((conde ((== P ()))
	      ((fresh (X R1 R2)
		      (conso X R1 P)
		      (conso X R2 L)
		      (prefix R1 R2)
		      )))
       s))))

(run () Q (prefix Q '(a b c d)))
(run () Q (suffix Q '(a b c d)))

(defun (proper-listo L)
  (lambda (s)
    (lambda ()
      ((conde ((== L ()))
	      ((fresh (X Xs)
		      (conso X Xs L)
		      (proper-listo Xs))))
       s))))

(run 5 Q (proper-listo Q))
(run () Q (proper-listo (cons Q 'x)))
