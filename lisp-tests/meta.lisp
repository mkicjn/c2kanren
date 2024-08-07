; Simple meta-circular evaluator test
(define assoc
  (lambda (s l)
    (cond ((not l) ())
	  ((eq (car (car l)) s) (cdr (car l)))
	  (t (assoc s (cdr l))))))

(define evlis
  (lambda (l env)
    (cond ((not l) ())
	  ((atom l) (meta-eval l env))
	  (t (cons (meta-eval (car l) env) (evlis (cdr l) env))))))

(define pairlis
  (lambda (a b env)
    (cond ((not a) env)
	  ((atom a) (cons (cons a b) env))
	  (t (cons (cons (car a) (car b))
		   (pairlis (cdr a) (cdr b) env))))))

(define evcon
  (lambda (cs env)
    (cond ((not cs) ())
	  ((meta-eval (car (car cs)) env) (meta-eval (car (cdr (car cs))) env))
	  (t (evcon (cdr cs) env)))))

(define apply
  (lambda (f args env)
    (cond ((eq (car (car f)) 'lambda)
	   (meta-eval (car (cdr (cdr (car f))))
		      (pairlis (car (cdr (car f))) (evlis args env) (cdr f))))
	  ((eq (car (car f)) 'label)
	   (apply (cons (car (cdr (cdr (car f))))
			(cons (cons (car (cdr (car f))) f) (cdr f)))
		  args env)))))

(define meta-eval
  (lambda (expr env)
    (cond ((eq 'symbol (type expr)) (assoc expr env))
	  ((atom expr) expr)
	  ((eq 'quote (car expr)) (car (cdr expr)))
	  ((eq 'cond (car expr)) (evcon (cdr expr) env))
	  ((eq 'car (car expr)) (car (meta-eval (car (cdr expr)) env)))
	  ((eq 'cdr (car expr)) (cdr (meta-eval (car (cdr expr)) env)))
	  ((eq 'atom (car expr)) (atom (meta-eval (car (cdr expr)) env)))
	  ((eq 'eq (car expr)) (eq (meta-eval (car (cdr expr)) env)
				   (meta-eval (car (cdr (cdr expr))) env)))
	  ((eq 'cons (car expr)) (cons (meta-eval (car (cdr expr)) env)
				       (meta-eval (car (cdr (cdr expr))) env)))
	  ((eq 'lambda (car expr)) (cons expr env))
	  ((eq 'label (car expr)) (cons expr env))
	  (t (apply (meta-eval (car expr) env) (cdr expr) env)))))

(meta-eval '((lambda (x) (cons x b)) (cons 0 a)) '((a . 1) (b . 2))) ; ((0 . 1) . 2)

(meta-eval '((label f (lambda () (f)))) '((a . 1))) ; infinite recursion
