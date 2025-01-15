; Executes on every launch, if located

;; Useful definitions

(define not (lambda (x) (eq x ())))

(define list (lambda args args))

(define curry (lambda (f x) (lambda args (f x . args))))

(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define bind (lambda (k v e) (cons (cons k v) e)))

(define assoc
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car (car l))) (car l))
	  (t (assoc s (cdr l))))))

(define map
  (lambda (f l)
    (cond (l (cons (f (car l)) (map f (cdr l)))))))


;; Support for macros
; TODO: Uncomment when tested with ukanren.lisp and friends

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

(define expand-rules
  (list
    (cons 'defun (lambda (defun name/args body)
		   (list 'define
			 (car name/args)
			 (list 'lambda (cdr name/args) (expand body)))))
    (cons 'defmacro (lambda (defmacro name/args body)
		      (list 'define
			    'expand-rules
			    (list 'bind
				  (list 'quote (car name/args))
				  (list 'lambda name/args (expand body))
				  'expand-rules))))
    (cons 'quote list) ; No recursion of `expand` implies no macro expansion within quotes
    ))

(define expand (lambda (term) (expand-all expand-rules term)))


;; Support for `let`

(defun (expand-let terms body)
  (cond ((not terms) (expand body))
	(t (list (list 'lambda
		       (list (car (car terms)))
		       (expand-let (cdr terms) body))
		 (expand (car (cdr (car terms))))))))

(defmacro (let terms body) (expand-let terms body))


;; Support for quasiquotation (or something like it)

(defun (ident x) x)

(defun (append-cont l1 l2 cont)
  (cond ((not l1) (cont l2))
	(t (append-cont (cdr l1) l2 (lambda (x) (cont (cons (car l1) x)))))))

(defun (append l1 l2)
  (append-cont l1 l2 ident))

(defun (expand-qq l)
  (cond ((not l) ())
	((atom l) (list 'quote l))
	((eq ',. (car l)) (expand (car (cdr l))))
	((eq ', (car l)) (list 'cons (expand (car (cdr l))) (expand-qq (cdr (cdr l)))))
	((eq ',@ (car l)) (list 'append (expand (car (cdr l))) (expand-qq (cdr (cdr l)))))
	(t (list 'cons (expand-qq (car l)) (expand-qq (cdr l))))))

(defmacro (` . l) (expand-qq l))
