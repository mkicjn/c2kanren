; Supporting non-Kanren definitions
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))
(define list (lambda args args))

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

(defmacro ` l
  ((Y (lambda (rec)
	(lambda (l)
	  (cond ((not l) ())
		((atom l) (list quote l))
		((eq ',@ (car l)) (list append (cadr l) (rec (cddr l))))
		((eq ', (car l)) (list cons (cadr l) (rec (cddr l))))
		(t (list cons (rec (car l)) (rec (cdr l)))))))) l))

;(assoc 0 '((2 . cat) (1 . 2) (0 . 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	uKanren
;
; Commented-out code represents either tests or intermediate definitions that were upgraded later
;
; The general flow of these definitions roughly follows this talk by uKanren creators Daniel Friedman and Jason Hemann:
; https://www.youtube.com/watch?v=0FwIwewHC3o
;
; Other interesting or excellent resources include:
; * The original uKanren paper, which helped serve as a reference / view into a similar but slightly different approach:
;     http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
; * "Unifying the Technical Interview", a fascinating article that drove the insipiration for this project and served as another reference:
;     https://aphyr.com/posts/354-unifying-the-technical-interview
;

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

;(define cats
;  (lambda (x)
;    (lambda (s/c)
;      (delay ((disj (== x 'cat) (cats x)) s/c)))))

;(take 4 (pull ((call/fresh (lambda (x) (cats x))) init-s/c)))

;(define cats-or-turtles
;  (lambda (x)
;    (lambda (s/c)
;      (delay ((disj (cats x) (turtles x)) s/c)))))

;(take 4 (pull ((call/fresh (lambda (x) (cats-or-turtles x))) init-s/c)))

; TODO: appendo

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some macros to make things a little easier

(define relation
  (macro (args body)
	 (list lambda args
	       (list lambda '(s/c)
		     (list delay (list body 's/c))))))

(define dogs (relation (x) (disj (== x 'dog) (dogs x))))
(define cats (relation (x) (disj (== x 'cat) (cats x))))

(define fresh1
  (macro (arg body)
	 (list call/fresh (list lambda (list arg) body))))

;(take 4 (pull ((fresh1 x (dogs x)) init-s/c)))

(define run (lambda (n g) (take n (pull (g init-s/c)))))

;(run 4 (fresh1 x (dogs x)))
(run 4 (fresh1 x (disj (dogs x) (cats x))))

; TODO: add reification to run

;(define expand
;  (lambda (args body)
;    (cond ((not args) body)
;	  (t (list 'call/fresh (list 'lambda (list (car args)) (expand (cdr args) body)))))))

;(expand '(x y z) '(conj (== x 'cat) (== y 'dog) (== z 'turtle)))

;(define fresh (macro (args body) (expand args body)))

;(run 1 (fresh (x y z) (conj (== x 'cat) (conj (== y 'dog) (== z 'turtle)))))

; TODO: Find a better way to define recursive macros
;(define fresh
;  (macro (args body)
;	 (let ((expand (lambda (self args body)
;			 (cond ((not args) body)
;			       (t (list call/fresh
;					(list lambda (list (car args))
;					      (self self (cdr args) body))))))))
;	   (expand expand args body))))

;(define fresh
;  (macro args
;	 ((Y (lambda (expand)
;	      (lambda (args body)
;		(cond ((not args) body)
;		      (t (list call/fresh
;			       (list lambda (list (car args))
;				     (expand (cdr args) body)))))))) . args)))

(defmacro fresh (args body)
  ((Y (lambda (expand)
	(lambda (args body)
	  (cond ((not args) body)
		(t (` call/fresh (lambda , args , (expand (cdr args body)))))))))
   args body))

(run 1 (fresh (x y z) (conj (== x 'cat) (conj (== y 'dog) (== z 'turtle)))))
