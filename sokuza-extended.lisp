; Sokuza-Kanren style implementation
; (Obviously, credit to https://github.com/miniKanren/sokuza-kanren.git)

; Also added streams and `occurs` check, to make it more like μKanren
; TODO: "birth record" optimization for `walk`
; TODO: disequality constraint `=/=`


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

(defun (var x) (cons '_ x))

(defun (_) (var ()))

(defun (var? x)
  (if (not (atom x))
    (eq (car x) '_)))


; Constraint Sets

(defun (bind s v c)
  (cons (cons s v) c))

(defun (unbind s c)
  (cond ((not c) ())
	((eq s (caar c)) (unbind s (cdr c)))
	(t (cons (car c) (unbind s (cdr c))))))

(defun (meta-get s c)
  (cdr (assoc s c)))

(defun (meta-set s v c)
  (cons (cons s v) (unbind s c)))


; Unification

(defun (get== k c)
  (let ((s (meta-get '== c)))
    (if (not (var? k)) k
      (let ((v (assoc k s)))
	(if (not v) k (get== (cdr v) c))))))

(defun (occurs v x c)
  (cond ((eq v x) t)
	((atom x) ())
	(t (if (occurs v (get== (car x) c) c) t
	     (occurs v (get== (cdr x) c) c)))))

(defun (set== x y c)
  (let ((s (meta-get '== c)))
    (if (occurs x y c) '#f
      (meta-set '== (bind x y s) c))))

(defun (unify x y c)
  (let ((x (get== x c))
	(y (get== y c)))
    (cond ((eq x y) c)
	  ((var? x) (set== x y c))
	  ((var? y) (set== y x c))
	  ((atom x) '#f)
	  ((atom y) '#f)
	  (t (let ((c (unify (car x) (car y) c)))
	       (if (eq c '#f) '#f
		 (unify (cdr x) (cdr y) c)))))))

(defun (== x y)
  (lambda (c)
    (let ((c (unify x y c)))
      (if (eq c '#f) () (list c)))))


; Connectives

(defun (disj g1 g2)
  (lambda (c)
    (stream-join (g1 c) (g2 c))))

(defun (conj g1 g2)
  (lambda (c)
    (stream-map g2 (g1 c))))


; Reification

(defun (reifier x)
  (lambda (c)
    (let ((x (get== x c)))
      (cond ((var? x) x)
	    ((atom x) x)
	    (t (cons ((reifier (car x)) c)
		     ((reifier (cdr x)) c)))))))


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
  (lambda (c)
    (lambda ()
      ((conde ((== As ()) (== Bs AsBs))
	      ((fresh (A s sBs)
		      (conso A s As)
		      (conso A sBs AsBs)
		      (appendo s Bs sBs)))) c))))

(run 6 A (appendo A (_) '(a b c d e)))
(run 6 A (appendo (_) A '(a b c d e)))
(run 6 (A B) (appendo A B '(a b c d e)))
(run 6 (A B C) (appendo A B C))
(run () A (appendo A A '(a b c a b c)))


(defun (evalo E R)
  (lambda (c)
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
		 (evalo X X`)))) c))))

(run 5 Q (evalo Q '(a b c)))
