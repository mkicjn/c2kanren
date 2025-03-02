; Executes on every launch, if located

;; Useful definitions

(define not (lambda (x) (eq x ())))

(define list (lambda args args))

(define curry (lambda (f x) (lambda args (f x . args))))

(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define assp
  (lambda (f l)
    (cond ((atom l) ())
	  ((f (car l)) (car l))
	  (t (assp f (cdr l))))))

(define assoc
  (lambda (s l)
    (assp (lambda (x) (eq s (car x))) l)))

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


;; Implement expand and defmacro

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


;; Support for let-bindings

(define expand-let
  (lambda (bindings body)
    (cons (list 'lambda (map (lambda (x) (car x)) bindings) body)
	  (map (lambda (x) (car (cdr x))) bindings))))

(define expand-let*
  (lambda (bindings body)
    (cond ((not bindings) body)
	  (t (list (list 'lambda
			 (list (car (car bindings)))
			 (expand-let* (cdr bindings) body))
		   (car (cdr (car bindings))))))))

(defmacro (let  bindings body) (expand-let  bindings body))
(defmacro (let* bindings body) (expand-let* bindings body))


;; Support for defun, with default args

(define arg-names
  (lambda (args)
    (cond ((atom args) args)
	  ((atom (car args)) (cons (car args) (arg-names (cdr args))))
	  (t (cons (car (car args)) (arg-names (cdr args)))))))

(define arg-defaults
  (lambda (args)
    (cond ((atom args) ())
	  ((atom (car args)) (arg-defaults (cdr args)))
	  (t (cons (car args) (arg-defaults (cdr args)))))))

(defmacro (defun name/args body)
  (let* ((name (car name/args))
	 (args (cdr name/args))
	 (names (arg-names args))
	 (defaults (arg-defaults args))
	 (lam (list 'lambda names body)))
    (cond (defaults (list 'define name (list 'let defaults lam)))
	  (t        (list 'define name lam)))))


;; List append and flatten

(defun (ident x) x)

(defun (append-cps cont l1 l2 . ls)
  (cond ((atom l1) (cond ((atom ls) (cont l2))
			 (t (append-cps cont l2 . ls))))
	(t (append-cps (lambda (x) (cont (cons (car l1) x))) (cdr l1) l2 . ls))))

(defun (append . ls)
  (cond ((atom ls) ())
	((atom (cdr ls)) (car ls))
	(t (append-cps ident () () . ls))))

(defun (flatten ls) (append . ls))


;; Support for quasiquotation

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
	(t (let ((_ (gensym)))
	     (` let ((, _ , first)) (cond (, _ , _) (t (or ,. rest))))))))


;; Right and left fold; better map; reverse
; TODO: Implement folds earlier?

(defun (fold-right f i l (cont ident))
  (cond ((not l) (cont i))
	((atom l) (cont l))
	(t (fold-right f i (cdr l) (lambda (x) (cont (f (car l) x)))))))

(defun (fold-left f i l)
  (cond ((not l) i)
	((atom l) l)
	(t (fold-left f (f i (car l)) (cdr l)))))

(defun (map f l)
  (fold-right (lambda (x y) (cons (f x) y)) () l))

(defun (reverse l)
  (fold-left (lambda (x y) (cons y x)) () l))

;; A fun way to implement `append` - but it's way slower than the original
;(defun (append . ls)
;  (fold-right (lambda (a b) (fold-right (lambda (x y) (cons x y)) b a)) () ls))


;; Zip

(defun (zip a b (cont ident))
  (cond ((atom a) (cont ()))
	((atom b) (cont ()))
	(t (zip (cdr a) (cdr b)
		(lambda (x) (cont (cons (cons (car a) (car b)) x)))))))


;; List access macros

(defmacro (caar l) (` car (car , l)))
(defmacro (cadr l) (` car (cdr , l)))
(defmacro (cdar l) (` cdr (car , l)))
(defmacro (cddr l) (` cdr (cdr , l)))
(defmacro (cadar l) (` car (cdar , l)))
(defmacro (caddr l) (` car (cddr , l)))


;; let+

(defun (locs l (cont (lambda () '_)))
  (cond ((not l) ())
	((atom l) (list (list l (cont))))
	(t (append (locs (car l) (lambda () (list 'car (cont))))
		   (locs (cdr l) (lambda () (list 'cdr (cont))))))))

(defmacro (destructuring-bind where tree body)
  (let ((_ (gensym)))
    (` let ((, _ , tree)) (let , (locs where (lambda () _)) , body))))

; Interesting example of recursive expansion
(defmacro (let+ exprs body)
  (cond ((atom exprs) body)
	((atom (caar exprs))
	 (` let (, (car exprs))
	    (let+ , (cdr exprs) , body)))
	(t (` destructuring-bind
	      , (caar exprs)
	      , (cadar exprs)
	      (let+ , (cdr exprs) , body)))))


;; List equality

(defun (equal a b)
  (cond ((eq a b) t)
	((atom a) ())
	((atom b) ())
	(t (and (equal (car a) (car b))
		(equal (cdr a) (cdr b))))))
