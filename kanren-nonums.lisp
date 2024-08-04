;; uKanren using pointer equality comparisons only for variables, i.e., not using numbers at all

;; Supporting non-Kanren definitions
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))
(define list (lambda args args))
(define curry (lambda (f x) (lambda args (f x . args))))

(define defun (macro (name args body) (list define name (list lambda args body))))
(define defmacro (macro (name args body) (list define name (list macro args body))))

(defun cadr (x) (car (cdr x)))
(defun cddr (x) (cdr (cdr x)))

(defun pair? (x) (not (atom x)))

(defun assoc (k l)
  (cond ((atom l) ())
	((eq k (car (car l))) (car l))
	(t (assoc k (cdr l)))))

(defun append (a b)
  (cond ((not a) b)
	(t (cons (car a) (append (cdr a) b)))))

(defmacro delay (x) (` lambda () , x))
(defmacro force (x) (` , x))

(defmacro ` l
  ((Y (lambda (rec)
	(lambda (l)
	  (cond ((not l) ())
		((atom l) (list quote l))
		((eq ',. (car l)) (cadr l))
		((eq ', (car l)) (list cons (cadr l) (rec (cddr l))))
		((eq ',@ (car l)) (list append (cadr l) (rec (cddr l))))
		(t (list cons (rec (car l)) (rec (cdr l)))))))) l))

(defun map (f l)
  (cond ((not l) ())
	(t (cons (f (car l)) (map f (cdr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	uKanren
;;

(defun make-var (x) (cons '_ x))
(defun var? (x) (eq (car x) '_))
(define var=? eq)

(defun walk (v s)
  (cond ((not (var? v)) v)
	(t (let ((b (assoc v s)))
	     (cond (b (walk (cdr b) s))
		   (t v))))))

(let ((a (make-var 0)) (b (make-var 1)))
  (walk a (` (, b . cat) (, a ,. b))))

(defun occurs? (v x s)
  (let ((x (walk x s)))
    (cond
      ((var? x) (eq v x))
      ((pair? x)
       (or (occurs? v (car x) s)
	   (occurs? v (cdr x) s)))
      (t ()))))

(defun ext-s (v x s)
  (cond ((occurs? v x s) ())
	(t (cons (cons v x) s))))

(defun unify (a b s)
  (let ((a (walk a s)) (b (walk b s)))
    (cond
      ((eq a b) s)
      ((var? a) (ext-s a b s))
      ((var? b) (ext-s b a s))
      ((and (pair? a) (pair? b))
       (let ((s1 (unify (car a) (car b) s)))
	 (and s1 (unify (cdr a) (cdr b) s1))))
      (t ()))))

; (let ((a (make-var 0)) (b (make-var 1))) (unify (cons a 'cat) b (` (, a . dog))))

(define mzero ())
(defun unit (s) (cons s mzero))
(define init-state ())

(defun == (a b)
  (lambda (s)
    (let ((s1 (unify a b s)))
      (cond (s1 (unit s1))
	    (t mzero)))))

; (let ((a (make-var 0)) (b (make-var 1))) ((== (cons a 'cat) b) (` (, a . dog))))

(defun call/fresh (f) (f (make-var ())))

(defun disj2 (g1 g2)
  (lambda (s)
    (mplus (g1 s) (g2 s))))

(defun conj2 (g1 g2)
  (lambda (s)
    (bind g2 (g1 s))))

(defun promise? (x) (eq (type x) 'lambda))

(defun mplus (s1 s2)
  (cond ((not s1) s2)
	((promise? s1) (mplus s2 (force s1)))
	(t (cons (car s1) (mplus (cdr s1) s2)))))

(defun bind (g s)
  (cond ((not s) ())
	((promise? s) (bind g (force s)))
	(t (mplus (g (car s)) (bind g (cdr s))))))


;; (End of core uKanren)

; (define cat-or-dog (lambda (x) (disj2 (== x 'cat) (== x 'dog)))) ((call/fresh cat-or-dog) ())

(defun take* (s)
  (cond ((promise? s) (take* (force s)))
	((atom s) s)
	(t (cons (car s) (take* (cdr s))))))

; (define cat-or-dog (lambda (x) (lambda (s) (delay ((disj2 (== x 'cat) (== x 'dog)) s))))) (take* ((call/fresh cat-or-dog) ()))

(defmacro relation (args body)
  (` lambda , args
     (lambda (s)
       (delay (, body s)))))

(defun decons (f l)
  (cond ((not l) ())
	(t (f (car l) (decons f (cdr l))))))

; (decons (lambda (a d) (or (and d (` x , a , d)) a)) '(1 2 3 4 5))

(defmacro conj args
  (decons (lambda (a d) (or (and d (` conj2 , a , d)) a)) args))

(defmacro disj args
  (decons (lambda (a d) (or (and d (` disj2 , a , d)) a)) args))

(defmacro fresh1 (arg body)
  (` call/fresh (lambda (, arg) , body)))

(defmacro fresh (args . body)
  (decons (lambda (a d) (` fresh1 , a , (or d (` conj ,. body)))) args))

(defmacro conde ls
  (let ((do-conj (lambda (l) (` conj ,. l)))
	(do-disj (lambda (l) (` disj ,. l))))
    (do-disj (map do-conj ls))))

; (fresh (x y z) (disj2 (== x 'cat) (== x 'dog)) (== y 'turtle) (== z 'something)) (take* ((fresh (x y z) (disj2 (== x 'cat) (== x 'dog)) (== y 'turtle) (== z 'something)) ()))

(defun reify (v s)
  (let ((v (walk v s)))
    (cond ((atom v) v)
	  (t (cons (reify (car v) s) (reify (cdr v) s))))))

; (let ((r (make-var 'reify-target))) (map (curry reify r) (take* (((lambda (v) (lambda (s) (delay ((fresh (x) (disj (== x 'cat) (== x 'dog)) (== x v)) s)))) r) init-state))))

(defun reify-targets (args)
  (cond ((not args) ())
	(t (cons (make-var (car args)) (reify-targets (cdr args))))))

; (reify-targets '(a b c d))

; (let ((args '(a b)) (targets (reify-targets args))) (map (curry reify targets) (take* (((relation (a b) (conde ((== a 'cat) (== b 'dog)) ((== a 'turtle) (== b 'lizard)))) . targets) init-state))))

(defmacro run* (args body)
  (` let ((targets (reify-targets (quote , args))))
     (map (curry reify targets) (take* (((relation , args , body) . targets) init-state)))))

; (run* (a b) (conde ((== a 'cat) (== b 'dog)) ((== a 'turtle) (== b 'lizard))))

(define appendo
  (relation (as bs as-bs)
	    (conde ((== as ()) (== bs as-bs))
		   ((fresh (a s s-bs)
			   (== as (cons a s))
			   (== as-bs (cons a s-bs))
			   (appendo s bs s-bs))))))

(run* (a b) (appendo a b '(A B C D E F G)))
