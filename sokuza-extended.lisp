; Sokuza-Kanren style implementation
; With extensions:
; * Streams
; * "Occurs" check
; * Disequality constraints

; Credits:
; https://github.com/miniKanren/sokuza-kanren.git
; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
; https://github.com/webyrd/dissertation-single-spaced


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
	(t (stream-join (g (car s)) (stream-map g (cdr s))))))


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

(defun (cfold c l f)
  (cond ((not l) c)
	((eq c 'fail) 'fail)
	(t (cfold (f c (car l)) (cdr l) f))))


; Unification

(defun (walk k c)
  (let ((e (meta-get '== c)))
    (if (not (var? k)) k
      (let ((v (assoc k e)))
	(if (not v) k (walk (cdr v) c))))))

(defun (occurs v x c)
  (cond ((eq v x) t)
	((atom x) ())
	(t (if (occurs v (walk (car x) c) c) t
	     (occurs v (walk (cdr x) c) c)))))

(defun (ext-== x y c)
  (let ((e (meta-get '== c)))
    (if (occurs x y c) 'fail
      (==->=/= x y (meta-set '== (bind x y e) c)))))

(defun (unify x y c)
  (let ((x (walk x c))
	(y (walk y c)))
    (cond ((eq x y) c)
	  ((var? x) (ext-== x y c))
	  ((var? y) (ext-== y x c))
	  ((atom x) 'fail)
	  ((atom y) 'fail)
	  (t (let ((c (unify (car x) (car y) c)))
	       (if (eq c 'fail) 'fail
		 (unify (cdr x) (cdr y) c)))))))

(defun (== x y)
  (lambda (c)
    (let ((c (unify x y c)))
      (if (eq c 'fail) () (list c)))))


; Connectives

(defun (disj g1 g2)
  (lambda (c)
    (stream-join (g1 c) (g2 c))))

(defun (conj g1 g2)
  (lambda (c)
    (stream-map g2 (g1 c))))


; Disequality Constraint
; TODO: Reification of constraints

(defun (-suffix l s)
  (cond
    ((not l) ())
    ((eq l s) ())
    (t (cons (car l) (-suffix (cdr l) s)))))

(defun (disunify x y c)
  (let ((e (meta-get '== c))
	(d (meta-get '=/= c))
	(c2 (unify x y c)))
    (if (eq c2 'fail) c
      (let* ((e2 (meta-get '== c2))
	     (p (-suffix e2 e))
	     (d2 (append p d)))
	(if (eq p ()) 'fail
	  (meta-set '=/= d2 c))))))

(defun (=/= x y)
  (lambda (c)
    (let ((c (disunify x y c)))
      (if (eq c 'fail) () (list c)))))

(defun (==->=/= x v c)
  (cfold c (meta-get '=/= c)
	 (lambda (c b)
	   (if (eq (car b) x)
	     (disunify v (cdr b) c)
	     c))))


; Reification

(defun (reifier x)
  (lambda (c)
    (let ((x (walk x c)))
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

(defmacro (run n q . gs)
  (` fresh , (if (atom q) (list q) q)
     (map (reifier , (if (atom q) q (cons 'list q)))
	  (take , n (, ((chain 'conj) gs) '())))))


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
		      (appendo s Bs sBs))))
       c))))

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
	 ((fresh (X X`)
		 (== E (` car , X))
		 (caro X` R)
		 (evalo X X`)))
	 ((fresh (X X`)
		 (== E (` cdr , X))
		 (cdro X` R)
		 (evalo X X`)))
	 ((fresh (A A` B B`)
		 (== E (` cons , A , B))
		 (== R (cons A` B`))
		 (evalo A A`)
		 (evalo B B`)))
	 ((fresh (X X`)
		 (== E (` atom , X))
		 (conde ((== R t) (=/= X` (cons (_) (_))))
			((== R ()) (== X` (cons (_) (_)))))
		 (evalo X X`)))
	 ((fresh (X Y X` Y`)
		 (== E (` eq , X , Y))
		 (conde ((== R ()) (=/= X` Y`))
			((== R  t) (==  X` Y`)))
		 (evalo X X`)
		 (evalo Y Y`))))
       c))))

(run 5 Q (evalo Q '(a b c)))
(run 10 Q (evalo (` eq t (_)) Q))


(defun (membero X L)
  (lambda (c)
    (lambda ()
      ((conde ((== L (cons X (_))))
	      ((fresh (Ls)
		      (== L (cons (_) Ls))
		      (membero X Ls))))
       c))))

(run () Q (=/= Q 'a) (disj (== Q 'a) (== Q 'b)))
(run () Q (disj (== Q 'a) (== Q 'd)) (=/= Q 'a))

(run () A (=/= A 'b) (=/= A 'd) (membero A '(a b c d e)))
