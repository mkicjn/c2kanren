; Experiments with trying to transpile Lisp code to C
; (does not generate working code)

(define list (lambda args args))
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cadar (lambda (x) (car (cdr (car x)))))
(define caddr (lambda (x) (car (cdr (cdr x)))))

(define join
  (lambda (s x)
    (cond ((cdr x) (cons (car x) (cons s (join s (cdr x)))))
	  (x (list (car x))))))

(define map
  (lambda (f x)
    (cond (x (cons (f (car x)) (map f (cdr x)))))))

(define transpile-cond
  (lambda (l)
    (cond ((not l) 'NULL)
	  ((eq (caar l) t) (transpile (cadar l)))
	  (t (list (transpile (caar l)) '? (transpile (cadar l)) ': (transpile-cond (cdr l)))))))

(define in
  (lambda (s l)
    (cond ((not l) ())
	  ((eq s (car l)) t)
	  (t (in s (cdr l))))))

(define transpile
  (lambda (x)
    (cond 
      ((not x) 'NULL)
      ((eq x t) 'sym_t)
      ((atom x) x)
      ((eq (car x) 'eq) (list (transpile (cadr x)) '== (transpile (caddr x))))
      ((eq (car x) 'quote) (list 'quote (list '" (cadr x) '")))
      ((eq (car x) 'lambda) (list 'lambda (cadr x) '{ 'return (transpile (caddr x)) '}))
      ((eq (car x) 'cond) (transpile-cond (cdr x)))
      ((eq (car x) 'not) (list '! (transpile (cadr x))))
      ((in (car x) '(car cdr cons)) (list (car x) (join ', (map transpile (cdr x)))))
      (t (list 'apply (join ', (map transpile x))))
      )))


(transpile '(cons a b))

(transpile '(eq (car a) b))

(transpile '(eq (car '(a b)) ()))

(transpile '(lambda (x) (car (cdr x))))

(transpile '(f x y z))

(transpile '(cond ((not a) t) (t ())))

(transpile (quote
(lambda (x)
    (cond 
      ((not x) 'NULL)
      ((eq x t) 'sym_t)
      ((atom x) x)
      ((eq (car x) 'eq) (list (transpile (cadr x)) '== (transpile (caddr x))))
      ((eq (car x) 'quote) (list 'quote (list '" (cadr x) '")))
      ((eq (car x) 'lambda) (list 'lambda (cadr x) '{ 'return (transpile (caddr x)) '}))
      ((eq (car x) 'cond) (transpile-cond (cdr x)))
      ((eq (car x) 'not) (list '! (transpile (cadr x))))
      ((in (car x) '(car cdr cons)) (list (car x) (join ', (map transpile (cdr x)))))
      (t (list 'apply (join ', (map transpile x))))
      ))
))
