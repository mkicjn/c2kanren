; With "inlining", this Collatz sequence benchmark is about as fast as the CHICKEN interpreter
(defmacro (inline f) (` defmacro (, f . args) (cons , (eval f) args)))

(inline +)
(inline -)
(inline *)
(inline /)
(inline mod)
(inline max)
(inline =)

; Also included: a small demo of default variables
(define collatz
  (let ((acc 0))
    (lambda (n acc) (cond ((= n 1) acc)
			  ((= (mod n 2) 0) (collatz (/ n 2) (+ acc 1)))
			  (t (collatz (+ 1 (* 3 n)) (+ acc 1)))))))

(define collatz-max
  (let ((m 0))
    (lambda (n m) (cond ((= n 1) m)
			(t (collatz-max (- n 1) (max (collatz n) m)))))))

(collatz-max 1000000)
