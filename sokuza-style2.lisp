; Sokuza-Kanren style implementation
; (Obviously, credit to https://github.com/miniKanren/sokuza-kanren.git)
; With streams, to make it more like μKanren

; Streams

(defun (functionp f)
  (cond ((atom f) ())
	((atom (car f)) ())
	((eq (caar f) 'lambda) t)))

(defun (take n s)
  (cond ((> 1 n) ())
	((atom s) ())
	((functionp s) (take n (s)))
	(t (cons (car s) (take (- n 1) (cdr s))))))

(defun (stream-join s1 s2)
  (cond ((not s1) s2)
	((functionp s1) (lambda () (stream-join s2 (s1))))
	(t (cons (car s1) (stream-join (cdr s1) s2)))))

(defun (stream-map g s)
  (cond ((not s) ())
	((functionp s) (lambda () (stream-map g (s))))
	(t (append (g (car s)) (stream-map g (cdr s))))))


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
	  ((atom x) '#f)
	  ((atom y) '#f)
	  (t (let ((env (unify (car x) (car y) env)))
	       (if (eq env '#f) '#f
		 (unify (cdr x) (cdr y) env)))))))

(defun (== x y)
  (lambda (s)
    (let ((s (unify x y s)))
      (if (eq s '#f) () (list s)))))


; Connectives

(defun (disj g1 g2)
  (lambda (s)
    (stream-join (g1 s) (g2 s))))

(defun (conj g1 g2)
  (lambda (s)
    (stream-map g2 (g1 s))))


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

(defmacro (run n q g)
  (` fresh , (if (atom q) (list q) q)
     (map (reifier , (if (atom q) q (cons 'list q)))
	  (take , n (, g '())))))


; Examples

(defun (conso A D C) (== (cons A D) C))
(defun (caro C A) (fresh (D) (conso A D C)))
(defun (cdro C D) (fresh (A) (conso A D C)))

(defun (appendo As Bs AsBs)
  (lambda (env)
    (lambda ()
      ((conde ((== As ()) (== Bs AsBs))
	      ((fresh (A s sBs)
		      (conso A s As)
		      (conso A sBs AsBs)
		      (appendo s Bs sBs)))) env))))

(run 6 A (fresh (B) (appendo A B '(a b c d e))))
(run 6 (A B) (appendo A B '(a b c d e)))
(run 6 (A B C) (appendo A B C))
(run () A (appendo A A '(a b c a b c)))


(defun (evalo E R)
  (lambda (env)
    (lambda ()
      ((conde
	 ((== E t) (== R t))
	 ((== E ()) (== R ()))
	 ((== E (` quote , R)))
	 ((fresh (A A` B B`)
		 (== E (` cons , A , B))
		 (== R (cons A` B`))
		 (evalo A A`)
		 (evalo B B`)))
	 ((fresh (X X`)
		 (== E (` car , X))
		 (caro X` R)
		 (evalo X X`)))
	 ((fresh (X X`)
		 (== E (` cdr , X))
		 (cdro X` R)
		 (evalo X X`)))) env))))

(run 5 Q (evalo Q '(a b c)))
