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

(define length
  (let ((acc 0))
    (lambda (l acc)
      (cond ((not l) acc)
	    (t (length (cdr l) (+ acc 1)))))))

(define iota
  (let ((i 0))
    (lambda (lim i)
      (cond ((>= i lim) ())
	    (t (cons i (iota lim (+ i 1))))))))

(defun zip (a b)
  (and a b (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	uKanren
;;
;; The general flow of these definitions roughly follows this talk by uKanren creators Daniel Friedman and Jason Hemann:
;; https://www.youtube.com/watch?v=0FwIwewHC3o
;;
;; Other interesting or excellent resources include:
;; * The original uKanren paper, which helped serve as a reference / view into a similar but slightly different approach:
;;     http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf
;; * "Unifying the Technical Interview", a fascinating article that drove the insipiration for this project and served as another reference:
;;     https://aphyr.com/posts/354-unifying-the-technical-interview
;;

; Variables are numbers
(define var (lambda (x) x))
(define var?  (lambda (x) (eq (type x) 'number)))

; `walk` follows a chain of bindings for v (variable) in s (substitutions)
(defun walk (v s)
  (cond
    ; If v is not a variable, we're done
    ((not (var? v)) v)
    ; Otherwise, find binding b for v in s
    (t (let ((b (assoc v s)))
	 ; If b exists, continue the walk; otherwise, v has no further bindings
	 (cond (b (walk (cdr b) s))
	       (t v))))))

; `occurs?` checks if variable v occurs inside the structure of x (could be anything) in s
(defun occurs? (v x s)
  ; Walk x in s
  (let ((x (walk x s)))
    (cond
      ; If x is just a variable, v occurs in x if they are equal
      ((var? x) (eq v x))
      ; If x is a pair, check if v occurs in the car or cdr of x
      ((pair? x)
       (or (occurs? v (car x) s)
	   (occurs? v (cdr x) s)))
      ; Otherwise, v does not occur in x
      (t ()))))

; `ext-s` extends the substitution s to include a new binding from v to x
(defun ext-s (v x s)
  ; If creating this binding would result in a cycle, fail by returning the empty list
  (cond ((occurs? v x s) ())
	; Otherwise cons the new pair to the existing s
	(t (cons (cons v x) s))))

; `unify` attempts to add substitutions to make a and b equivalent in s, returning a new substitution list
(defun unify (a b s)
  ; Walk a and b in s
  (let ((a (walk a s)) (b (walk b s)))
    (cond
      ; If they are the same, s does not need to be updated
      ((eq a b) s)
      ; If either one is a variable, s can be trivially extended
      ((var? a) (ext-s a b s))
      ((var? b) (ext-s b a s))
      ; If they are both lists, unify car and cdr
      ((and (pair? a) (pair? b))
       ; Unify cars in s to form s1
       (let ((s1 (unify (car a) (car b) s)))
	 ; On success, return the result from further unifying cdrs in s1
	 (and s1 (unify (cdr a) (cdr b) s1))))
      ; Otherwise, unification is impossible
      (t ()))))

; TODO: Explain s/c in more detail

; The empty stream is an empty list
(define mzero ())
; A single s/c pair can be lifted into a stream by simply putting it into a list
(define unit (lambda (s/c) (cons s/c mzero)))

; `==` constructs a goal (a function of s/c -> s/c) unifying a and b
(defun == (a b)
  (lambda (s/c)
    ; Destructure s/c as (s . c)
    (let ((s (car s/c)) (c (cdr s/c)))
      ; If a and b unify in s to form s1, return new s/c (s1 . c)
      (let ((s1 (unify a b s)))
	(cond (s1 (unit (cons s1 c)))
	      ; Otherwise, return the empty subst list
	      (t mzero))))))

; The initial subst/counter pair is (() . 0)
(define init-s/c (cons mzero 0))

; `call/fresh` takes a unary goal constructing function and constructs a new goal that calls the function with a new variable obtained by incrementing the counter, then executes it with the updated counter
(defun call/fresh (f)
  (lambda (s/c)
    ; Destructure s/c
    (let ((s (car s/c)) (c (cdr s/c)))
      ; Call the goal with a new variable and updated s/c
      ((f (var c)) (cons s (+ c 1))))))

; `disj` forms a logical disjunction by appending the s/c pairs returned by two goals
(defun disj (g1 g2)
  (lambda (s/c)
    (mplus (g1 s/c) (g2 s/c))))

; `conj` forms a logical conjunction by calling one goal on the s/c pairs returned by another
(defun conj (g1 g2)
  (lambda (s/c)
    (bind g2 (g1 s/c))))

; A "promise" is a lambda function that should be called to get more s/c pairs
(define promise? (lambda (x) (eq (type x) 'lambda)))

; TODO: Explain streams in more detail

; `mplus` combines two streams for disj
(defun mplus (stream1 stream2)
  (cond
    ; If stream1 is empty, return stream2
    ((not stream1) stream2)
    ; If stream1 is a promise, return a promise which gets the next s/c from stream1 but continues from stream2
    ((promise? stream1) (mplus stream2 (force stream1)))
    ; Otherwise, do a simple list append
    (t (cons (car stream1) (mplus (cdr stream1) stream2)))))

; `mplus` applies a goal to a stream for conj
(defun bind (goal stream)
  (cond
    ; If the stream is empty, that's all
    ((not stream) ())
    ; If the stream is a promise, return a promise recursing on the next stream value
    ((promise? stream) (bind goal (force stream)))
    ; Otherwise, combine the streams formed by calling goal on the first result and recursing over the rest
    (t (mplus (goal (car stream)) (bind goal (cdr stream))))))

; TODO: Continue adding comments

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some macros to make things a little easier

(defmacro relation (args body)
  (` lambda , args
     (lambda (s/c)
       (delay (, body s/c)))))

(defun decons (f l)
  (cond ((not l) ())
	(t (f (car l) (decons f (cdr l))))))

(defmacro conj+ exprs
  (decons (lambda (a d) (or (and d (` conj , a , d)) a)) exprs))

(defmacro disj+ exprs
  (decons (lambda (a d) (or (and d (` disj , a , d)) a)) exprs))

(defmacro conde ls
  (let ((do-conj (lambda (l) (` conj+ ,. l)))
	(do-disj (lambda (l) (` disj+ ,. l))))
    (do-disj (map do-conj ls))))

(defmacro fresh1 (arg body)
  (` call/fresh (lambda (, arg) , body)))

(defmacro fresh (args . body)
  (decons (lambda (a d) (` fresh1 , a , (or d (` conj+ ,. body)))) args))

;; Obtaining results from a stream

(defun pull (stream)
  (cond ((promise? stream) (pull (force stream)))
	(t stream)))

(defun take* (stream)
  (cond ((not (car stream)) ())
	(t (cons (car stream) (take* (pull (cdr stream)))))))

(defun take (n stream)
  (cond ((not (car stream)) ())
	((= n 1) (list (car stream)))
	(t (cons (car stream) (take (- n 1) (pull (cdr stream)))))))


;; Reification

(defun reify (v s/c)
  (let ((s (car s/c)) (c (cdr s/c)) (v (walk v s)))
    (cond ((atom v) v)
	  (t (cons (reify (car v) s/c) (reify (cdr v) s/c))))))


;; "Run" macros for executing goals ergonomically

(defmacro run* (vs g)
  (` map (curry reify (iota , (length vs)))
     (take* (pull ((fresh , vs , g) init-s/c)))))

(defmacro run (n vs g)
  (` map (curry reify (iota , (length vs)))
     (take , n (pull ((fresh , vs , g) init-s/c)))))


;; appendo relation and test

(define appendo
  (relation (as bs as-bs)
	    (conde ((== as ()) (== bs as-bs))
		   ((fresh (a s s-bs)
			   (== as (cons a s))
			   (== as-bs (cons a s-bs))
			   (appendo s bs s-bs))))))

(run* (a b) (appendo a b '(A B C D E F G)))

;; TODO: Finish adding comments
;; TODO: Is it worth combining the commented versions in with the old version to illustrate the development over time?
;; TODO: Go further - attributed variables? evalo??
