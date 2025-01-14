; Experiments with expanding macros at read time
; (i.e., to eventually do macros better than the interpreter provides)


;; Supporting definitions (probably preloaded)

(define list (lambda args args))

(define curry
  (lambda (f x)
    (lambda args (f x . args))))

(define bind
  (lambda (k v e)
    (cons (cons k v) e)))

(define assoc
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car (car l))) (car l))
	  (t (assoc s (cdr l))))))

(define map
  (lambda (f l)
    (cond (l (cons (f (car l)) (map f (cdr l)))))))


;; Implementing term expansion

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

; Testing term expansion

(expand-all (list (cons 'left (lambda (left x y) x))
		  (cons 'right (lambda (right x y) y)))
	    '(cons (left a b) (right a b)))


;; Implementing some real macros

(define expand-rules
  (list
    (cons 'defun (lambda (defun name/args body)
		   (list 'define
			 (car name/args)
			 (list 'lambda (cdr name/args) body))))
    (cons 'defmacro (lambda (defmacro name/args body)
		      (list 'define
			    'expand-rules
			    (list 'bind
				  (list 'quote (car name/args))
				  (list 'lambda name/args body)
				  'expand-rules))))
    (cons 'quote list) ; Do not expand within quotes
    ))

(define expand (lambda (term) (expand-all expand-rules term)))

; Testing those macros

(defmacro (left x y) x)
(defmacro (right x y) y)

(cons (left 'a 'b) (right 'a 'b))

(defun (assoc s l)
  (cond ((not l) ())
	((eq s (car (car l))) (car l))
	(t (assoc s (cdr l)))))

(assoc 'b '((a . 1) (b . 2) (c . 3)))

(defmacro (middle a b c) b)

(cons (middle 'a 'b 'c) (right 'b 'c))


;; Implementing `let` as a macro

(define expand-let
  (lambda (terms body)
    (cond ((not terms) body)
	  (t (list (list 'lambda
			 (list (car (car terms)))
			 (expand-let (cdr terms) body))
		   (car (cdr (car terms))))))))

(defmacro (let terms body) (expand-let terms body))

; Testing `let` implementation

(expand-let '((a 'a) (b 'b)) '(cons a b))

(let ((a 'a) (b 'b) (c (list a b)) (d (cdr c))) d)


;; Porting quasiquote macro to this syntax

(defun (ident x) x)

(defun (append-cont l1 l2 cont)
  (cond ((not l1) (cont l2))
	(t (append-cont (cdr l1) l2 (lambda (x) (cont (cons (car l1) x)))))))

(defun (append l1 l2)
  (append-cont l1 l2 ident))

(defun (expand-qq l)
  (cond ((not l) ())
	((atom l) (list 'quote l))
	((eq ',. (car l)) (car (cdr l)))
	((eq ', (car l)) (list 'cons (car (cdr l)) (expand-qq (cdr (cdr l)))))
	((eq ',@ (car l)) (list 'append (car (cdr l)) (expand-qq (cdr (cdr l)))))
	(t (list 'cons (expand-qq (car l)) (expand-qq (cdr l))))))

(defmacro (` . l) (expand-qq l))

; Testing CPS append and quasiquote macro

(append '(1 2 3) '(4))

(define a '(1 2 3))
(define b '5)

(` ,@ a 4 ,. b)
