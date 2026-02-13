; Experiments with expanding macros at read time
; (i.e., to eventually do macros better than the interpreter provides)

(define expand ()) ; Suppress rcfile macro expansion, if present, which could break below code

;; Supporting definitions (probably preloaded)

(define list (lambda args args))

(define curry
  (lambda (f x)
    (lambda args (f x . args))))

(define assoc
  (lambda (s l)
    (if (eq l ()) ()
      (if (eq s (car (car l))) (car l)
	(assoc s (cdr l))))))

(define map
  (lambda (f l)
    (if (eq l ()) ()
      (if (atom l) (f l)
	(cons (f (car l)) (map f (cdr l)))))))


;; Implementing macro expansion

(define non-expandable
  (lambda (expr)
    (if (atom expr) t
      (if (eq (car expr) 'quote) t
	()))))

(define expand-shallow
  (lambda (rules expr)
    (if (non-expandable expr) expr
      ((lambda (rule)
	 (if (eq rule ()) expr
	   (expand-shallow rules (rule . expr))))
       (cdr (assoc (car expr) rules))))))

(define expand-deep
  (lambda (rules expr)
    ((lambda (expr)
       (if (non-expandable expr) expr
	 (map (curry expand-deep rules) expr)))
     (expand-shallow rules expr))))

; Testing macro expansion

(define test-rules
  (list (cons 'left (lambda (left x y) x))
	(cons 'right (lambda (right x y) y))))

(expand-shallow test-rules '(right a (left b c)))

(expand-deep test-rules '(cons (left a b) (right a (left b c))))

(expand-deep test-rules '(cons (left a b) '(right a (left b c))))


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
; ^ Note: Using (curry expand-deep expand-rules) doesn't work.
; This closes over the initial value of expand-rules, preventing updates.

(defmacro (defun name/args body)
  (list 'define
	(car name/args)
	(list 'lambda (cdr name/args) body)))

; Translate `not` into (eq _ ()) and cond into `if` chain
; (This was added at a later point for compatibility with a newer interpreter)
(defmacro (not x) (list 'eq () x))
(defmacro (cond . qs-and-as)
  (if (atom qs-and-as) ()
    (if (eq t (car (car qs-and-as)))
      (car (cdr (car qs-and-as)))
      (list 'if (car (car qs-and-as)) (car (cdr (car qs-and-as)))
	    (cons 'cond (cdr qs-and-as))))))

; Testing those macros

(defmacro (left x y) x)
(defmacro (right x y) y)

(right 'a 'b)

(cons (left 'a 'b) (right 'a 'b))

(defun (assoc s l)
  (cond ((not l) ())
	((eq s (car (car l))) (car l))
	(t (assoc s (cdr l)))))

(assoc 'b '((a . 1) (b . 2) (c . 3)))

(defmacro (middle a b c) b)

(cons (middle 'a 'b 'c) (right 'a (left 'b 'c)))


;; Implementing `let` as a macro

(defun (expand-let bindings body)
  (cond ((not bindings) body)
	(t (list (list 'lambda
		       (list (car (car bindings)))
		       (expand-let (cdr bindings) body))
		 (car (cdr (car bindings)))))))

(defmacro (let bindings body) (expand-let bindings body))

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
(define b 6)

(expand-qq '(,@ a 4 , (left 5 6) ,. b))
(expand (expand-qq '(,@ a 4 , (left '5 '6) ,. b)))
(` ,@ a 4 , (left 5 6) ,. b)
