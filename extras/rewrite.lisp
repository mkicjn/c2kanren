; Toy term rewriter (WIP)

(defmacro (let? binds body)
  (if (atom binds) body
    (let ((var (caar binds))
	  (val (cadar binds))
	  (rest (cdr binds)))
      (` let ((, var , val))
	 (if (eq , var 'fail) 'fail
	   (let? , rest , body))))))

;(expand '(let? ((a (f 1)) (b (f 2))) (cons a b)))

(defun (unify rule-lhs term (head t))
  (cond (head (if (not (eq (car rule-lhs) (car term))) 'fail
		(unify (cdr rule-lhs) (cdr term) ())))
	((not rule-lhs) (if (not term) () 'fail))
	((atom (car rule-lhs))
	 (let? ((dbinds (unify (cdr rule-lhs) (cdr term) ())))
	       (cons (cons (car rule-lhs) (car term)) dbinds)))
	((atom (car term)) 'fail)
	(t (let? ((abinds (unify (car rule-lhs) (car term) t))
		  (dbinds (unify (cdr rule-lhs) (cdr term) ())))
		 (append abinds dbinds)))))

;(unify '(car (cons a b)) '(car (cons (atom x) (equal y z))))

(defun (lookup sym env)
  (let ((bind (assoc sym env)))
    (if bind bind 'fail)))

(defun (rephrase rule-rhs env (head t))
  (cond ((not rule-rhs) ())
	((atom rule-rhs)
	 (let? ((bind (lookup rule-rhs env)))
	       (cdr bind)))
	(head (let? ((tail (rephrase (cdr rule-rhs) env ())))
		    (cons (car rule-rhs) tail)))
	(t (let? ((head (rephrase (car rule-rhs) env t))
		  (tail (rephrase (cdr rule-rhs) env ())))
		 (cons head tail)))))

;(rephrase 'a (unify '(car (cons a b)) '(car (cons (atom x) (equal y z)))))

(defun (rewrite rule term)
  (let ((lhs (car rule))
	(rhs (cadr rule)))
    (let? ((env (unify lhs term))
	   (term` (rephrase rhs env)))
	  term`)))

;(rewrite '((car (cons a b)) a) '(car (cons (atom x) (equal y z))))

(defun (try-rewrites rules term)
  (if (not rules) ()
    (let ((term` (rewrite (car rules) term))
	  (tail (try-rewrites (cdr rules) term)))
      (if (eq term` 'fail) tail
	(cons term` tail)))))

(define *rewrite-rules*
  '(((car (cons a b)) a)
    ((cdr (cons a b)) b)
    ((and (and a b) c) (and a (and b c)))
    ((and t b) b)
    ((and () b) ())
    ((or (or a b) c) (or a (or b c)))
    ((or () b) b)
    ((atom (cons a b)) ())
    ((if t a b) a)
    ((if () a b) b)))

(try-rewrites *rewrite-rules* '(car (cons (atom x) (equal y z))))
