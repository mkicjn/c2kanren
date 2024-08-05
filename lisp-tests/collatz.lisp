; This Collatz sequence benchmark runs about 16% faster here than in the CHICKEN interpreter
; It's still about 87x slower than paraforth, though ;)

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
