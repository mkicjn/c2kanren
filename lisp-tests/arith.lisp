; Experimenting with implementing arithmetic symbolically

(define xor (lambda (a b) (cond (a (cond (b ()) (t t)))
				(b (cond (a ()) (t t))))))

(define add-bit   (lambda (a b c) (xor c (xor a b))))
(define carry-bit (lambda (a b c) (or (and (xor a b) c) (and a b))))

(define list (lambda args args))

(define add-bits-c
  (lambda (l1 l2 c)
    (cond
      ((and (not l1) (not l2)) (cond (c (list c))))
      (t (cons (add-bit (car l1) (car l2) c)
	       (add-bits-c (cdr l1) (cdr l2) (carry-bit (car l1) (car l2) c)))))))

(define invert
  (lambda (l)
    (cond (l (cons (not (car l)) (invert (cdr l)))))))

(define add-bits
  (lambda (l1 l2)
    (add-bits-c l1 l2 ())))

(define subtract-bits
  (lambda (l1 l2)
    (add-bits-c l1 (invert l2) t)))

(add-bits '(t t () t ()) '(() t t () ()))

(add-bits '(t t t t) '(t))

(invert '(t t () t () t () ()))
