; Toy term rewriter (WIP)
; Note: Not rigorously defined - duplicate symbols will cause problems

(defun (walk x env)
  (if (not (atom x)) x
    (let ((m (assoc x env)))
      (if (not m) x
	(walk (cdr m) env)))))

(defun (unify x y (h t) (env ()))
  (let ((x (walk x env)))
    (cond 
      ((eq x y) env)
      ((not x) (if (not y) env 'fail))
      ((atom x) (cons (cons x y) env))
      ((atom y) 'fail)
      (h (if (not (eq (car x) (car y))) 'fail
	   (unify (cdr x) (cdr y) () env)))
      (t (let ((env (unify (car x) (car y) t env)))
	   (if (eq env 'fail) 'fail
	     (unify (cdr x) (cdr y) () env)))))))

(defun (rephrase x env (h t))
  (cond ((eq env 'fail) 'fail)
	((not x) ())
	((atom x) (walk x env))
	(h (cons (car x) (rewrite (cdr x) env ())))
	(t (let ((xa (rewrite (car x) env t))
		 (xd (rewrite (cdr x) env ())))
	     (cons xa xd)))))

(defun (rewrite x rule (prop t))
  (match rule
	 ((equal , lhs , rhs)
	  (rephrase rhs (unify lhs x)))
	 ((iff , lhs , rhs)
	  (if (not prop) 'fail
	    (rephrase rhs (unify lhs x))))))

;(rewrite '(car (cons x y)) '(equal (car (cons a b)) a)) 

(defun (try-rewrites x rules prop)
  (if (not rules) 'fail
    (let ((xp (rewrite x (car rules) prop)))
      (if (not (eq xp 'fail)) xp
	(try-rewrites x (cdr rules) prop)))))

(define *rewrite-rules*
  '((equal (car (cons a b)) a)
    (equal (cdr (cons a b)) b)
    (equal (and (and a b) c)
	   (and a (and b c)))
    (equal (and t b) b)
    (equal (and () b) ())
    (equal (or (or a b) c)
	   (or a (or b c)))
    (equal (or () b) b)
    (equal (atom (cons a b)) ())
    (equal (if t a b) a)
    (equal (if () a b) b)
    ))

;(try-rewrites '(car (cons x y)) *rewrite-rules* ())

(defun (map-rewrite x rules prop)
  (if (atom x) 'fail
    (let ((a (try-rewrites (car x) rules prop)))
      (if (not (eq a 'fail))
	(cons a (cdr x))
	(let ((d (map-rewrite (cdr x) rules ())))
	  (if (not (eq d 'fail))
	    (cons (car x) d)
	    'fail))))))

(let ((r (lambda (x) (map-rewrite x *rewrite-rules* ()))))
  (r (r '(cons (car (cons d e)) (cdr (cons f g))))))
