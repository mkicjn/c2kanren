; Quasiquotation macro demo
(define list (lambda args args))
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define defun (macro (name args body) (list define name (list lambda args body))))
(define defmacro (macro (name args body) (list define name (list macro args body))))

(defun cadr (x) (car (cdr x)))
(defun caar (x) (car (car x)))
(defun cadar (x) (cadr (car x)))

(defun append (a b)
  (cond ((not a) b)
	(t (cons (car a) (append (cdr a) b)))))

(defmacro quasi (l)
  ((Y (lambda (rec)
	(lambda (l)
	  (cond ((not l) ())
		((atom l) (list quote l))
		((eq 'splice (caar l)) (list append (cadar l) (rec (cdr l))))
		((eq 'unquote (car l)) (cadr l))
		(t (list cons (rec (car l)) (rec (cdr l)))))))) l))

(define a 1)
(define b '(2 3))
(quasi ((unquote (- 2 1)) (splice b) 4)) ; (1 2 3 4)
