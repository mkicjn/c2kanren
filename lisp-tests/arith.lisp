; Experimenting with implementing arithmetic symbolically

(defun (none l) (cond ((not l) t) ((car l) ()) (t (none (cdr l)))))
(defun (one l) (cond ((not l) ()) ((car l) (none (cdr l))) (t (one (cdr l)))))

(define xor (lambda ls (one ls)))

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

(subtract-bits '(t t t t) '(t () () ()))

(invert '(t t () t () t () ()))


(define lsh1 (lambda (x) (cons () x)))
(define rsh1 (lambda (x) (cdr x)))
(define lsb (lambda (x) (car x)))

(define multiply-bits-acc
  (lambda (a b acc)
    (cond ((not b) acc)
	  ((lsb b) (multiply-bits-acc (lsh1 a) (rsh1 b) (add-bits a acc)))
	  (t       (multiply-bits-acc (lsh1 a) (rsh1 b) acc)))))

(define multiply-bits
  (lambda (a b)
    (multiply-bits-acc a b ())))

(multiply-bits '(t t t) '(() t t))


(define decimal-values
  '((0 . ())
    (1 . (t))
    (2 . (() t))
    (3 . (t t))
    (4 . (() () t))
    (5 . (t () t))
    (6 . (() t t))
    (7 . (t t t))
    (8 . (() () () t))
    (9 . (t () () t))
    (10 . (() t () t))))

(define decimal-bits (lambda (x) (cdr (assoc x decimal-values))))

(define ten (decimal-bits '10))

(define parse-decimal-acc
  (lambda (digits acc)
    (cond ((not digits) acc)
	  (t (parse-decimal-acc (cdr digits)
				(add-bits (decimal-bits (car digits))
					  (multiply-bits acc ten)))))))

(define parse-decimal (lambda (digits) (parse-decimal-acc digits ())))

(parse-decimal '(1 2))

(parse-decimal '(4 2))

(multiply-bits (parse-decimal '(1 2 3)) (parse-decimal '(4 5 6)))
