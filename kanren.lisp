(define pair? (lambda (x) (not (atom x))))
(define list (lambda args args))
(define assoc
  (lambda (k l)
    (cond ((atom l) ())
	  ((eq k (car (car l))) (car l))
	  (t (assoc k (cdr l))))))

;(assoc 0 '((2 . cat) (1 . 2) (0 . 1)))

(define var (lambda (x) x))
(define var?  (lambda (x) (eq (type x) 'number)))

(define walk
  (lambda (v s)
    (cond ((not (var? v)) v)
	  (t (let ((b (assoc v s)))
	       (cond (b (walk (cdr b) s))
		     (t v)))))))

;(walk 0 '((2 . cat) (1 . 2) (0 . 1)))
;(walk 3 '((2 . cat) (1 . 2) (0 . 1)))

(define occurs?
  (lambda (x v s)
    (cond ((var? v) (eq x v))
	  ((pair? v)
	   (or (occurs? x (walk (car v) s) s)
	       (occurs? x (walk (cdr v) s) s)))
	  (t ()))))

(define ext-s
  (lambda (v x s)
    (cond ((occurs? v x) ())
	  (t (cons (cons v x) s)))))

;(ext-s 3 'dog '((2 . cat) (1 . 2) (0 . 1)))

(define unify
  (lambda (a b s)
    (cond ((eq a b) s)
	  ((var? a) (ext-s a b s))
	  ((var? b) (ext-s b a s))
	  ((and (pair? a) (pair? b))
	   (let ((s1 (unify (walk (car a) s) (walk (car b) s) s)))
	     (and s1 (unify (walk (cdr a) s1) (walk (cdr b) s1) s1))))
	  (t ()))))

;(unify 1 0 '((2 . cat) (1 . 2) (0 . 1)))
;(unify 3 'dog '((2 . cat) (1 . 2) (0 . 1)))
;(unify '(cat dog 1 dog) '(cat 3 0 dog) '((2 . cat) (1 . 2) (0 . 1)))
;(unify '(cat dog 1 dog) '(cat 3 cat 0) '((2 . cat) (1 . 2) (0 . 1)))
;(unify '(1 . 2) '(cat . cat) ())
;(unify '(1 . 2) '(cat . 1) ())

;(define ==
;  (lambda (a b)
;    (lambda (s)
;      (let ((s (unify a b s)))
;	(cond (s (list s))
;	      (t ()))))))

;(== '(2 . cat) '(1 . cat))
;((== '(2 . cat) '(1 . cat)) ())

(define mzero ())
(define unit (lambda (s/c) (cons s/c mzero)))

(define ==
  (lambda (a b)
    (lambda (s/c)
      (let ((s (car s/c)) (c (cdr s/c)))
	(let ((s1 (unify a b s)))
	  (cond (s1 (unit (cons s1 c)))
		(t mzero)))))))

(define init-s/c (cons mzero 0))

;(== '(2 . cat) '(1 . cat))
;((== '(2 . cat) '(1 . cat)) s/c0)

(define call/fresh
  (lambda (f)
    (lambda (s/c)
      (let ((s (car s/c)) (c (cdr s/c)))
	((f (var c)) (cons s (+ c 1)))))))

;((call/fresh (lambda (x) (== x 'cat))) init-s/c)

(define disj
  (lambda (g1 g2)
    (lambda (s/c)
      (mplus (g1 s/c) (g2 s/c)))))

;(define mplus
;  (lambda (stream1 stream2)
;    (cond ((not stream1) stream2)
;	  (t (cons (car stream1) (mplus (cdr stream1) stream2))))))

;(mplus '(1 2 3) '(4 5 6))

(define conj
  (lambda (g1 g2)
    (lambda (s/c)
      (bind g2 (g1 s/c)))))

;(define bind
;  (lambda (goal stream)
;    (cond ((not stream) ())
;	  (t (mplus (goal (car stream)) (bind goal (cdr stream)))))))

;(mplus '(1 2 3) '(4 5 6))

;((call/fresh (lambda (x) (disj (== x 'cat) (== x 'dog)))) init-s/c)

(define promise? (lambda (x) (eq (type x) 'lambda)))

(define delay (macro (x) (list lambda () x)))
(define force (macro (x) (list x)))

(define mplus
  (lambda (stream1 stream2)
    (cond ((not stream1) stream2)
	  ((promise? stream1) (delay (mplus stream2 (force stream1))))
	  (t (cons (car stream1) (mplus (cdr stream1) stream2))))))

(define bind
  (lambda (goal stream)
    (cond ((not stream) ())
	  ((promise? stream) (delay (bind goal (force stream))))
	  (t (mplus (goal (car stream)) (bind goal (cdr stream)))))))

;(force ((call/fresh (lambda (x) (lambda (s/c) (delay ((disj (== x 'cat) (== x 'dog)) s/c))))) init-s/c))

(define turtles
  (lambda (x)
    (lambda (s/c)
      (delay ((disj (== x 'turtle) (turtles x)) s/c)))))

;(force ((call/fresh (lambda (x) (turtles x))) init-s/c))
;(force (cdr (force ((call/fresh (lambda (x) (turtles x))) init-s/c))))
;(force (cdr (force (cdr (force ((call/fresh (lambda (x) (turtles x))) init-s/c))))))

(define pull
  (lambda (stream)
    (cond ((promise? stream) (pull (force stream)))
	  (t stream))))

(define take
  (lambda (n stream)
    (cond ((= n 1) (list (car stream)))
	  (t (cons (car stream) (take (- n 1) (pull (cdr stream))))))))

;(take 4 (pull ((call/fresh (lambda (x) (turtles x))) init-s/c)))

(define cats
  (lambda (x)
    (lambda (s/c)
      (delay ((disj (== x 'cat) (cats x)) s/c)))))

;(take 4 (pull ((call/fresh (lambda (x) (cats x))) init-s/c)))

(define cats-or-turtles
  (lambda (x)
    (lambda (s/c)
      (delay ((disj (cats x) (turtles x)) s/c)))))

(take 4 (pull ((call/fresh (lambda (x) (cats-or-turtles x))) init-s/c)))

; TODO: appendo
