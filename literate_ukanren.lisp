
; This Lisp dialect has some quirks related to runtime type checking,
; so we start with some auxiliary definitions to this effect.

(defun (functionp x)
  (cond ((atom x) ())
	((atom (car x)) ())
	((eq (caar x) 'lambda) t)))

(defun (listp x)
  (and (not (atom x)) (not (functionp x))))

(defun (numberp x)
  (+ x 0))

(defun (symbolp x)
  (and (atom x) (not (numberp x))))

; Also, some macros for enabling or disabling tests

(defmacro (test f r) (` equal , f (quote , r))) ; Enable tests
;(defmacro (test f r) '(define test-off t)) ; Disable tests


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 1: Streams and Stream Manipulation

; At its core, μKanren (and miniKanren) work by manipulating streams.

; What is a stream?
(defun (streamp x)
  (or
    ; We say a stream is basically a list...
    (listp x)
    ; ...or a "generator" function.
    (functionp x)))

; For simplicity, we can just check (atom x) to see if the stream is empty.

; So, how do we take an item out of a stream?
; First, we want the stream to be a list so we have something tangible to take.
(defun (advance-stream s)
  (cond
    ; If the stream is empty, there's nothing to take.
    ((atom s) ())
    ; If we encounter a generator function, we'll try calling it.
    ((functionp s) (advance-stream (s)))
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
    ((> n 0) (let ((s` (advance-stream s)))
	       (cons (car s`) (take (- n 1) (cdr s`)))))
    ; If nil was given, take as many as possible.
    ((not n) (let ((s` (advance-stream s)))
	       (cons (car s`) (take () (cdr s`)))))))

(test (take 3 '(1 2 3 4 5 6 7))
      (1 2 3))
(test (take 5 (seq 100 200))
      (100 101 102 103 104))
(test (take 10 (seq 15 20))
      (15 16 17 18 19 20))
(test (take () (seq 55 65))
      (55 56 57 58 59 60 61 62 63 64 65))

; OK, but suppose we want to combine streams. How do we do this?
; Let's try building a "higher-order" generator, a stream combinator.

; Let's try to make a "concatenate" combinator that takes two streams and returns another:
(defun (cat s1 s2)
  ; Inside our new stream:
  (lambda ()
    ; First, advance stream 1.
    (let ((s1` (advance-stream s1)))
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
; Let's try a small modification to get a new "alternating" stream combinator:

(defun (alt s1 s2)
  ; Same as cat...
  (lambda ()
    (let ((s1` (advance-stream s1)))
      (if (atom s1`) s2
	; ...EXCEPT: let stream 2 go next before continuing with stream 1.
	(cons (car s1`) (alt s2 (cdr s1`)))))))
;                       ^^^^^^^^^^^^^^^^^^

(test (take 10 (alt (seq 100 200) (seq 200 300)))
      (100 200 101 201 102 202 103 203 104 204))

; Looks better!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Part 2: TODO
