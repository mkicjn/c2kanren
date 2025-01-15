; This Collatz sequence benchmark used to run about 16% faster here than in the CHICKEN interpreter
; It was still about 87x slower than paraforth, though ;)
; TODO: Investigate why it's much slower now - definitions from rc.lisp slowing down the environment?

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
