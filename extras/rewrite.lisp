; Toy J-Bob style term rewriter (WIP)

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

(defun (rewrite-there rule term where)
  (if (not where) (rewrite rule term)
    (cond ((eq (car where) 'a)
	   (let? ((a (rewrite-there rule (car term) (cdr where))))
		 (cons a (cdr term))))
	  ((eq (car where) 'd)
	   (let? ((d (rewrite-there rule (cdr term) (cdr where))))
		 (cons (car term) d)))
	  (t 'fail))))

;(rewrite-there '((cdr (cons a b)) b)
;	       '(a (b (c d (cdr (cons (atom x) (equal y z))) e)) f)
;	       '(d a d a d d a))

(defun (rewrites-by-name names/rules term names/wheres)
  (if (atom names/wheres) term
    (let ((name (caar names/wheres))
	  (where (cdar names/wheres)))
      (let? ((name/rule (lookup name names/rules))
	     (term` (rewrite-there (cdr name/rule) term where)))
	    (rewrites-by-name names/rules term` (cdr names/wheres))))))

(define *rewrite-rules*
  '((car-cons (car (cons a b)) a)
    (cdr-cons (cdr (cons a b)) b)
    (and-assoc (and (and a b) c) (and a (and b c)))
    (and-t (and t b) b)
    (and-nil (and () b) ())
    (or-assoc (or (or a b) c) (or a (or b c)))
    (or-nil (or () b) b)
    (atom-cons (atom (cons a b)) ())
    (if-t (if t a b) a)
    (if-nil (if () a b) b)))

(rewrites-by-name
  *rewrite-rules*
  '(if (atom (cons x y))
     (cdr (cons (atom z) (equal w q)))
     (car (cons (f g) (h j))))
  '((atom-cons d a)
    (if-nil)
    (car-cons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun (try-rewrites names/rules term)
  (if (not names/rules) ()
    (let* ((rule (cdar names/rules))
	   (term` (rewrite rule term))
	   (tail (try-rewrites (cdr names/rules) term)))
      (if (eq term` 'fail) tail
	(cons term` tail)))))

(try-rewrites *rewrite-rules* '(car (cons (atom x) (equal y z))))
