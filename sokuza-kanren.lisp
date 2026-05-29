; Sokuza-Kanren style implementation
; (Obviously, credit to https://github.com/miniKanren/sokuza-kanren.git)

; Variables

(defun (var x)
  (cons '_ x))

(defun (var? x)
  (if (not (atom x))
    (eq (car x) '_)))


; Unification

(defun (lookup k env)
  (if (not (var? k)) k
    (let ((v (assoc k env)))
      (if (not v) k (lookup (cdr v) env)))))

(defun (unify x y env)
  (let ((x (lookup x env))
	(y (lookup y env)))
    (cond ((eq x y) env)
	  ((var? x) (cons (cons x y) env))
	  ((var? y) (cons (cons y x) env))
	  ((atom x) 'fail)
	  ((atom y) 'fail)
	  (t (let ((env (unify (car x) (car y) env)))
	       (if (eq env 'fail) 'fail
		 (unify (cdr x) (cdr y) env)))))))

(defun (== x y)
  (lambda (s)
    (let ((s (unify x y s)))
      (if (eq s 'fail) () (list s)))))


; Connectives

(defun (disj g1 g2)
  (lambda (s)
    (append (g1 s) (g2 s))))

(defun (conj g1 g2)
  (lambda (s)
    (apply append (map g2 (g1 s)))))


; Reification

(defun (reifier x)
  (lambda (s)
    (let ((x (lookup x s)))
      (cond ((var? x) x)
	    ((atom x) x)
	    (t (cons ((reifier (car x)) s)
		     ((reifier (cdr x)) s)))))))


; Interface

(defun (chain op)
  (lambda (l)
    (if (not (cdr l)) (car l)
      (list op (car l) ((chain op) (cdr l))))))

(defmacro (conde . clauses)
  ((chain 'disj) (map (chain 'conj) clauses)))

(defmacro (fresh vars . body)
  (` let , (map (lambda (v) (` , v (var (quote , v)))) vars)
     , ((chain 'conj) body)))

(defmacro (run q g)
  (` fresh , (if (atom q) (list q) q)
     (map (reifier , (if (atom q) q (cons 'list q)))
	  (, g '()))))

; Example

(defun (appendo As Bs AsBs)
  (lambda (env)
    ((conde ((== As ()) (== Bs AsBs))
	    ((fresh (A s sBs)
		    (== As (cons A s))
		    (== AsBs (cons A sBs))
		    (appendo s Bs sBs)))) env)))

(run (A B) (appendo A B '(a b c d e)))
