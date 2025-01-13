; Experiments with expanding macros at read time
; (i.e., to eventually do macros better than the interpreter provides)

(define assoc
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car (car l))) (car l))
	  (t (assoc s (cdr l))))))

(define list (lambda args args))

(define curry
  (lambda (f x)
    (lambda args (f x . args))))

(define map
  (lambda (f l)
    (cond (l (cons (f (car l)) (map f (cdr l)))))))


(define pick-rule
  (lambda (rules term base)
    (cond ((not rules) base)
	  ((eq (car term) (car (car rules))) (cdr (car rules)))
	  (t (pick-rule (cdr rules) term base)))))

(define expander
  (lambda (rules)
    (lambda term (map (curry expand rules) term))))

(define expand
  (lambda (rules term)
    (cond ((atom term) term)
	  (t ((pick-rule rules term (expander rules)) . term)))))


(expand (list (cons 'left (lambda (left x y) x))
	      (cons 'right (lambda (right x y) y)))
	'(cons (left a b) (right a b)))
