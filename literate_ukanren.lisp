; This Lisp dialect has some quirks related to runtime type checking,
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

; For simplicity, we can just check (atom x) to see if the stream is empty.

; So, how do we take an item out of a stream?
; First, we want the stream to be a list so we have something tangible to take.
(defun (next s)
  (cond
    ; If the stream is empty, there's nothing to take.
    ((atom s) ())
    ; If we encounter a generator function, we'll try calling it.
    ((function? s) (next (s)))
    ; Otherwise, we can already take one out in the obvious way.
    (t s)))

; For example, here's a function that returns a stream generating a sequence of numbers.
(defun (seq from to)
  (if (> to from)
    (cons from (lambda () (seq (+ from 1) to)))
    (list to)))

; Let's take some numbers out of it.
(defun (take n s)
  (cond
    ; If the stream is an atom (probably nil), there's nothing left to take.
    ((atom s) s)
    ; If a number greater than zero was given, count down from there.
    ((> n 0) (let ((s` (next s)))
	       (if s` (cons (car s`) (take (- n 1) (cdr s`))))))
    ; If nil was given, take as many as possible.
    ((not n) (let ((s` (next s)))
	       (if s` (cons (car s`) (take () (cdr s`))))))))

(test (take 3 '(1 2 3 4 5 6 7))
      (1 2 3))
(test (take 5 (seq 100 200))
      (100 101 102 103 104))
(test (take 10 (seq 15 20))
      (15 16 17 18 19 20))
(test (take () (seq 55 65))
      (55 56 57 58 59 60 61 62 63 64 65))

; OK, but suppose we want to combine streams. How do we do this?
; Let's try building a "higher-order" generator, a stream operator.

; Let's try to make a "concatenate" operator that takes two streams and returns another:
(defun (cat s1 s2)
  ; Inside our new stream:
  (lambda ()
    ; First, advance stream 1.
    (let ((s1` (next s1)))
      ; If it turns out to be empty, continue from stream 2.
      (if (atom s1`) s2
	; Otherwise, present the first item from stream 1 and repeat.
	(cons (car s1`) (cat (cdr s1`) s2))))))

(test (take 10 (cat '(1 2 3 4 5) '(11 12 13 14 15)))
      (1 2 3 4 5 11 12 13 14 15))

; It works!

(test (take 10 (cat (seq 100 200) (seq 200 300)))
      (100 101 102 103 104 105 106 107 108 109))

; Hmm... but we might like to see some results from both streams.
; Let's try a small modification to get a new "alternating" stream operator:

(defun (alt s1 s2)
  ; Same as cat...
  (lambda ()
    (let ((s1` (next s1)))
      (if (atom s1`) s2
	; ...EXCEPT: let stream 2 go next before continuing with stream 1.
	(cons (car s1`) (alt s2 (cdr s1`)))))))
;                       ^^^^^^^^^^^^^^^^^^

(test (take 10 (alt (seq 100 200) (seq 200 300)))
      (100 200 101 201 102 202 103 203 104 204))

; Looks better!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: Logic Variables and Unification

; Now for a slight detour - what exactly is a logic variable?

; Logic variables basically act like placeholders or "don't cares" in a Lisp expression.

; For lack of a better option, we'll let logic variables be a pair with '_' at the head.
; (The tail can hold any value at all.)
(defun (var (x '_)) (cons '_ x))
(defun (var? x)
  (if (not (atom x)) (eq (car x) '_)))

; We'll know that two variables are equal when their pointers are equal.
(defun (var= x y) (eq x y))

; We will want to be able to check if two lists can be made equal by assigning values to these.
; This process is called "unification."

; For example, (1 2 (_ . X) 4) unified with (1 2 3 4) will assign variable (_ . X) to the value 3.

; As variables are bound according to some environment, unification modifies its environment
; until it either succeeds (returning a new environment) or fails (returning nothing).

; First, we need a function to look up logic variables in an environment:
(defun (walk v e)
  ; If our variable is not a variable (anymore), then we're done.
  (if (not (var? v)) v
    ; Otherwise, treat the environment as an association list and do a lookup.
    (let ((x (assoc v e)))
      ; If we get nothing, give up and return the variable itself.
      ; Otherwise, keep looking, since assignments may be chained.
      (if (not x) v (walk (cdr x) e)))))

; TODO: Explain and add the "occurs check."

; Now we can write a function to perform unification:
(defun (unify x y e)
  ; First, try to get actual values for both arguments.
  (let ((x (walk x e))
	(y (walk y e))) ; <- TODO: Convince yourself the recursion in `walk` is actually necessary in light of these.
    (cond
      ; If the two are definitionally equal (via pointer comparison or numeric value), we can stop.
      ((eq x y) e)
      ; If one or the other is a logic variable, we can simply bind it to the other.
      ((var? x) (cons (cons x y) e))
      ((var? y) (cons (cons y x) e))
      ; Otherwise, if either one is an atom, the two cannot possibly have the same value.
      ; (If they did, we should have returned true already.)
      ((atom x) 'fail) ; <- TODO: Is there a cleaner way to distinguish empty environments from failure?
      ((atom y) 'fail)
      ; Lastly, we have the harder case where both arguments are lists.
      ; In this case, we try to unify the head, then the tail.
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
; Part 3: Streams + Unification = Goals

; Part 1 mentions that the miniKanren family of logic languages works by manipulating streams.
; But, streams of what? Streams of environments from Part 2, of course!

; Part 1 also demonstrated two generalized operators for streams: `cat` and `alt`.
; Part 3 introduces "goals," which are specialized operator for streams of environments.
; A mini/μKanren program is just a combination of these.

; The most fundamental goal constructor `==` modifies a stream of environments
; by attempting to unify its arguments in each environment:
(defun (== x y)
  ; Return a unary stream operator that...
  (lambda (s)
    ; ...extracts the head (e) and tail (es) of its input stream...
    (let ((s` (next s)))
      (if s`
	(let ((e (car s`)) (es (cdr s`)))
	  ; ...tries to unify its arguments in each environment from the stream...
	  (let ((e` (unify x y e))
		(gen (lambda () ((== x y) es)))) ; (preparing a generator for the rest)
	    ; ...and outputs the modified environment only when unification succeeds.
	    (if (eq e` 'fail) gen (cons e` gen))))))))

; ^ TODO: Worth refactoring this?

(test (take () (let ((X (var 'X)) (Y (var 'Y)))
		 ((== (` 1 , X 3) (` 1 2 , Y))
		  (` ()))))
      ((((_ . Y) . 3) ((_ . X) . 2))))

(test (take () (let ((X (var 'X)) (Y (var 'Y)))
	   ((== (` 5 , X 7 8) (` 5 6 , Y 8))
	    (` ((, X . 4))
	       ((, X . 5))
	       ((, X . 6))
	       ((, X . 7))
	       ((, X . 8))
	       ))))
      ((((_ . Y) . 7) ((_ . X) . 6))))


; One goal is not much of a logic program, however.
; To combine goals, we have two obvious connectives:
; * Conjunction (logical AND)
; * Disjunction (logical OR)

; Thankfully, these can be implemented intuitively with what we have.

; Conjunction: Pass the stream through one goal, then the other.
; This requires each environment in the output stream to pass through BOTH goals.
(defun (conj g1 g2)
  ; Take a stream...
  (lambda (s)
    ; ...and pass its contents through both goals.
    (g2 (g1 s))))

; TODO: Attempt at fair conjunction?

; Disjunction: Pass the stream through both goals and combine the results.
; This requires each environment in the output stream to pass through EITHER goal.
(defun (disj g1 g2)
  ; Take a stream...
  (lambda (s)
    ; ...pass it to both goals and combine them.
    (alt (g1 s) (g2 s))))

; Either `cat` or `alt` will work to combine the two streams in `disj`.
; Using `cat` may result in behavior closer to Prolog-style SLD clause resolution.
; However, `alt` may be preferable in general, in case the first goal diverges.

(test (take () ((let ((X (var 'X)) (Y (var 'Y)))
		  (conj (disj (== X 1) (== X 2))
			(disj (== Y 3) (== Y 4))))
		'(())))
      ((((_ . Y) . 3) ((_ . X) . 1))
       (((_ . Y) . 4) ((_ . X) . 1))
       (((_ . Y) . 3) ((_ . X) . 2))
       (((_ . Y) . 4) ((_ . X) . 2))))

; TODO: conde, fresh, reify, run
