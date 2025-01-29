; Useful definitions

(defmacro (caar x) (` car (car , x)))
(defmacro (cadr x) (` car (cdr , x)))
(defmacro (cdar x) (` cdr (car , x)))
(defmacro (cadar x) (` cadr (car , x)))
(defmacro (caddr x) (` cadr (cdr , x)))
(defmacro (caddar x) (` caddr (car , x)))

(defun (zip as bs)
  (cond ((not as) ())
	((not bs) ())
	(t (cons (cons (car as) (car bs)) (zip (cdr as) (cdr bs))))))

(defun (join s x)
  (cond ((cdr x) (cons (car x) (cons s (join s (cdr x)))))
	(x (list (car x)))))

(defun (in s l)
  (cond ((not l) ())
	((eq s (car l)) t)
	(t (in s (cdr l)))))


;; Functions for getting info from a define or lambda expression

(defun (func-name def/lambda)
  (cond ((atom def/lambda) ())
	((eq (car def/lambda) 'lambda) '?)
	((eq (car def/lambda) 'define) (cadr def/lambda))
	(t ())))

(defun (func-lambda def/lambda)
  (cond ((atom def/lambda) ())
	((eq (car def/lambda) 'lambda) def/lambda)
	((eq (car def/lambda) 'define) (caddr def/lambda))
	(t ())))

(defun (func-args def/lambda)
  (cadr (func-lambda def/lambda)))

(defun (func-body def/lambda)
  (caddr (func-lambda def/lambda)))


;; Extract all functions from an expression

(defun (extract-funcs expr)
  (let ((name (func-name expr)))
    (cond (name (cons (cons (func-lambda expr) name)
		      (extract-funcs (func-body expr))))
	  ((atom expr) ())
	  (t (append (extract-funcs (car expr))
		     (extract-funcs (cdr expr)))))))


; Testing on some code that uses functions in various ways

(define append-sample '(

(define ident (lambda (x) x))
(define append_cps
  (lambda (cont l1 l2)
    (cond ((not l1) (cont l2))
	  (t (append_cps (lambda (x) (cont (cons (car l1) x)))
		      (cdr l1) l2)))))
(define append (lambda (l1 l2) (append_cps ident l1 l2)))


))

(extract-funcs append-sample)


;; Assign valid identifier names to unnamed functions

(defun (rename-lambdas funcs names)
  (cond ((not funcs) ())
	((eq (cdar funcs) '?) (cons (cons (caar funcs) (car names))
				    (rename-lambdas (cdr funcs) (cdr names))))
	(t (cons (car funcs) (rename-lambdas (cdr funcs) names)))))


; Testing on the same sample as before

(define anon-names '(f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10))

(rename-lambdas (extract-funcs append-sample) anon-names)


;; Extract all free values from a function

(defun (in x xs)
  (cond ((not xs) ())
	((eq x (car xs)) t)
	(t (in x (cdr xs)))))

(defun (free-vars-acc args expr acc)
  (cond ((not expr) acc)
	((atom expr) (cond ((in expr args) acc)
			   ((in expr acc) acc)
			   (t (cons expr acc))))
	((eq (car expr) 'quote) acc)
	((eq (car expr) 'lambda)
	    (free-vars-acc (append (func-args expr) args)
			   (func-body expr) acc))
	(t (free-vars-acc args (car expr)
		      (free-vars-acc args (cdr expr) acc)))))

(defun (free-vars args expr) (free-vars-acc args expr ()))


; Testing free variable extraction

(define primitives '(car cdr cons t atom eq cond not))

(let* ((funcs (rename-lambdas (extract-funcs append-sample) anon-names))
       (func-names (map (lambda (x) (cdr x)) funcs))
       (all-names (append primitives func-names))
       (free-vars (map (lambda (x) (cons (cdr x) (free-vars all-names (car x)))) funcs)))
  (list funcs free-vars))


;; Transpile function contents

(defun (transpile-cond ls l)
  (cond ((not l) 'NULL)
	((eq (caar l) t) (transpile-expr ls (cadar l)))
	(t (list (transpile-expr ls (caar l)) '? (transpile-expr ls (cadar l))
		 ': (transpile-cond ls (cdr l))))))

(defun (transpile-expr ls x)
  (cond ((not x) 'NULL)
	((eq x t) 'sym_t)
	((atom x) x)
	((eq (car x) 'eq) (list (transpile-expr ls (cadr x)) '== (transpile-expr ls (caddr x))))
	((eq (car x) 'quote) (list 'quote (list '" (cadr x) '")))
	((eq (car x) 'lambda) (list 'CLOSURE (list (cdr (assoc x ls)))))
	((eq (car x) 'cond) (transpile-cond ls (cdr x)))
	((eq (car x) 'not) (list '! (transpile-expr ls (cadr x))))
	((in (car x) '(car cdr cons)) (list (car x) (join ', (map (curry transpile-expr ls) (cdr x)))))
	(t (list (transpile-expr ls (car x)) (join ', (map (curry transpile-expr ls) (cdr x)))))))

(defun (transpile-lambdas0 ls0 ls)
  (cond ((not ls) ())
	(t (let ((l (caar ls)) (name (cdar ls)))
	     (append (list 'void '* name (join ', (cadr l)) '{ 'return (transpile-expr ls0 (caddr l)) '})
		     (transpile-lambdas0 ls0 (cdr ls)))))))

(defun (transpile-lambdas ls) (transpile-lambdas0 ls ls))

 
; Testing on the same sample as before

(transpile-lambdas (rename-lambdas (extract-funcs append-sample) anon-names))
