; Experiments with expanding macros at read time
; (i.e., to eventually do macros better than the interpreter provides)

(define list (lambda args args))

(define curry
  (lambda (f x)
    (lambda args (f x . args))))

(define assoc
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car (car l))) (car l))
	  (t (assoc s (cdr l))))))

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
    (lambda term (map (curry expand-all rules) term))))

(define expand-all
  (lambda (rules term)
    (cond ((atom term) term)
	  (t ((pick-rule rules term (expander rules)) . term)))))


(expand-all (list (cons 'left (lambda (left x y) x))
		  (cons 'right (lambda (right x y) y)))
	    '(cons (left a b) (right a b)))

(define expand-rules
  (list
    (cons 'left (lambda (left x y) x))
    (cons 'right (lambda (right x y) y))
    (cons 'defun (lambda (defun name/args body)
		   (list 'define
			 (car name/args)
			 (list 'lambda
			       (cdr name/args)
			       body))))
    (cons 'defmacro (lambda (defmacro name/args body)
		      (list 'define
			    'expand-rules
			    (list 'cons
				  (list 'cons
					(list 'quote (car name/args))
					(list 'lambda name/args body))
				  'expand-rules))))
    (cons 'quote list)
    ))

(define expand (lambda (term) (expand-all expand-rules term)))

(cons (left 'a 'b) (right 'a 'b))

(defun (assoc s l)
  (cond ((not l) ())
	((eq s (car (car l))) (car l))
	(t (assoc s (cdr l)))))

(assoc 'b '((a . 1) (b . 2) (c . 3)))

(defmacro (middle a b c) b)

(cons (middle 'a 'b 'c) (right 'b 'c))
