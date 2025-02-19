; Executes on every launch, if located

;; Useful definitions

(define not (lambda (x) (eq x ())))

(define list (lambda args args))

(define curry (lambda (f x) (lambda args (f x . args))))

(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define assoc
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car (car l))) (car l))
	  (t (assoc s (cdr l))))))

(define map
  (lambda (f l)
    (cond ((not l) ())
	  ((atom l) (f l))
	  (t (cons (f (car l)) (map f (cdr l)))))))


;; Macro support

(define non-expandable
  (lambda (expr)
    (cond ((atom expr) t)
	  ((eq (car expr) 'quote) t)
	  (t ()))))

(define expand-shallow
  (lambda (rules expr)
    (cond ((non-expandable expr) expr)
	  (t ((lambda (rule)
		(cond ((not rule) expr)
		      (t (expand-shallow rules (rule . expr)))))
	      (cdr (assoc (car expr) rules)))))))

(define expand-deep
  (lambda (rules expr)
    ((lambda (expr)
       (cond ((non-expandable expr) expr)
	     (t (map (curry expand-deep rules) expr))))
     (expand-shallow rules expr))))


;; Implementing some real macros

(define expand-rules
  (list
    (cons 'defmacro
	  (lambda (defmacro name/args body)
	    (list 'define
		  'expand-rules
		  (list 'cons
			(list 'cons
			      (list 'quote (car name/args))
			      (list 'lambda name/args body))
			'expand-rules))))))

(define expand (lambda (expr) (expand-deep expand-rules expr)))

(defmacro (defun name/args body)
  (list 'define
	(car name/args)
	(list 'lambda (cdr name/args) body)))


;; Support for let-bindings

(defun (expand-let bindings body)
  (cons (list 'lambda (map (lambda (x) (car x)) bindings) body)
	(map (lambda (x) (car (cdr x))) bindings)))

(defun (expand-let* bindings body)
  (cond ((not bindings) body)
	(t (list (list 'lambda
		       (list (car (car bindings)))
		       (expand-let* (cdr bindings) body))
		 (car (cdr (car bindings)))))))

(defmacro (let  bindings body) (expand-let  bindings body))
(defmacro (let* bindings body) (expand-let* bindings body))


;; Support for quasiquotation

(defun (ident x) x)

(defun (append-cps cont l1 l2 . ls)
  (cond ((not l1) (cond ((not ls) (cont l2))
			(t (append-cps cont l2 . ls))))
	(t (append-cps (lambda (x) (cont (cons (car l1) x))) (cdr l1) l2 . ls))))

(defun (append . ls)
  (append-cps ident . ls))

(defun (expand-qq l)
  (cond ((not l) ())
	((atom l) (list 'quote l))
	((eq ',. (car l)) (car (cdr l)))
	((eq ', (car l)) (list 'cons (car (cdr l)) (expand-qq (cdr (cdr l)))))
	((eq ',@ (car l)) (list 'append (car (cdr l)) (expand-qq (cdr (cdr l)))))
	(t (list 'cons (expand-qq (car l)) (expand-qq (cdr l))))))

(defmacro (` . l) (expand-qq l))


;; Support for short-circuiting and/or

(defmacro (and first . rest)
  (cond ((not rest) first)
	(t (` cond (, first (and ,. rest)) (t ())))))

(defmacro (or first . rest)
  (cond ((not rest) first)
	(t (` let ((_ , first)) (cond (_ _) (t (or ,. rest)))))))
