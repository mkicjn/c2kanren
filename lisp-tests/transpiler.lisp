; Experiments with trying to transpile-expr ls Lisp code to C
; (does not generate working code)

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

(define join
  (lambda (s x)
    (cond ((cdr x) (cons (car x) (cons s (join s (cdr x)))))
	  (x (list (car x))))))

(define in
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car l)) t)
	  (t (in s (cdr l))))))

(define transpile-cond
  (lambda (ls l)
    (cond ((not l) 'NULL)
	  ((eq (caar l) t) (transpile-expr ls (cadar l)))
	  (t (list (transpile-expr ls (caar l)) '? (transpile-expr ls (cadar l))
		   ': (transpile-cond ls (cdr l)))))))

(define transpile-expr
  (lambda (ls x)
    (cond 
      ((not x) 'NULL)
      ((eq x t) 'sym_t)
      ((atom x) x)
      ((eq (car x) 'eq) (list (transpile-expr ls (cadr x)) '== (transpile-expr ls (caddr x))))
      ((eq (car x) 'quote) (list 'quote (list '" (cadr x) '")))
      ((eq (car x) 'lambda) (cdr (assoc x ls))) ;(list (cadr x) '{ 'return (transpile-expr ls (caddr x)) '}))
      ((eq (car x) 'cond) (transpile-cond ls (cdr x)))
      ((eq (car x) 'not) (list '! (transpile-expr ls (cadr x))))
      ((in (car x) '(car cdr cons)) (list (car x) (join ', (map (curry transpile-expr ls) (cdr x)))))
      (t (list 'apply (join ', (map (curry transpile-expr ls) x)))))))


(transpile-expr () '(cons a b))

(transpile-expr () '(eq (car a) b))

(transpile-expr () '(eq (car '(a b)) ()))

(let ((l '(lambda (x) (car (cdr x)))))
  (transpile-expr (list (cons l 'f0)) l))

(transpile-expr () '(f x y z))

(transpile-expr () '(cond ((not a) t) (t ())))

(transpile-expr () (caddar transpile-expr))


(define extract-lambdas
  (lambda (x)
    (cond ((atom x) ())
	  ((eq (car x) 'lambda) (cons x (extract-lambdas (caddr x))))
	  (t (append
	       (extract-lambdas (car x))
	       (extract-lambdas (cdr x)))))))

(define lambda-names '(f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19 f20))

(define bind-lambdas
  (lambda (expr)
    (zip (extract-lambdas expr) lambda-names)))


(extract-lambdas 
  '(define Y (lambda (f) (f (lambda args ((Y f) . args))))))

(bind-lambdas 
  '(define Y (lambda (f) (f (lambda args ((Y f) . args))))))


(define transpile-lambdas0
  (lambda (ls0 ls)
    (cond ((not ls) ())
	  (t (let ((l (caar ls)) (name (cdar ls)))
	       (append (list 'void '* name (cadr l) '{ 'return (transpile-expr ls0 (caddr l)) '})
		       (transpile-lambdas0 ls0 (cdr ls))))))))

(define transpile-lambdas
  (lambda (ls) (transpile-lambdas0 ls ls)))


(transpile-lambdas
  (bind-lambdas
    '(define Y (lambda (f) (f (lambda args ((Y f) . args)))))))
; ^ Argument pasting presents some serious challenges
