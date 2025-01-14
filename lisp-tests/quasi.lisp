; Quasiquotation macro demo
(define list (lambda args args))
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define cadr (lambda (x) (car (cdr x))))
(define cddr (lambda (x) (cdr (cdr x))))
(define caar (lambda (x) (car (car x))))
(define cadar (lambda (x) (cadr (car x))))

(define append
  (lambda (a b)
    (cond ((not a) b)
	  (t (cons (car a) (append (cdr a) b))))))

(define quasi
  (macro (l)
	 ((Y (lambda (rec)
	       (lambda (l)
		 (cond ((not l) ())
		       ((atom l) (list 'quote l))
		       ((eq 'splice (caar l)) (list 'append (cadar l) (rec (cdr l))))
		       ((eq 'unquote (car l)) (cadr l))
		       (t (list 'cons (rec (car l)) (rec (cdr l)))))))) l)))

; Even more sugary
(define `
  (macro l
	 ((Y (lambda (rec)
	       (lambda (l)
		 (cond ((not l) ())
		       ((atom l) (list 'quote l))
		       ((eq ',. (car l)) (cadr l))
		       ((eq ', (car l)) (list 'cons (cadr l) (rec (cddr l))))
		       ((eq ',@ (car l)) (list 'append (cadr l) (rec (cddr l))))
		       (t (list 'cons (rec (car l)) (rec (cdr l)))))))) l)))


(define a '1)
(define b '(2 3))

(quasi ((unquote '1) (splice b) 4)) ; (1 2 3 4)
(` , '1 ,@ b 4) ; (1 2 3 4)
(` , '1 ,@ b . 4) ; (1 2 3 . 4)
(` , '1 ,@ b ,. '4) ; (1 2 3 . 4)
