(defun (member l ls)
  (cond ((not ls) ())
	((eq l (car ls)) t)
	(t (member l (cdr ls)))))

(define binops '(+ - * / % > = cons eq))

(defun (curried term)
  (match term
	 (_ term when (atom term))
	 ((lambda (, arg) , body)
	  (` lambda (, arg) , (curried body)))
	 ((lambda (, first ,. rest) , body)
	  (` lambda (, first) , (curried (` lambda , rest , body))))
	 ((, op , a , b)
	  (` , op , (curried a) , (curried b)) when (member op binops))
	 ((if , t1 , t2 , t3)
	  (` if , (curried t1) , (curried t2) , (curried t3)))
	 ((, t1 ,. ts)
	  (fold-left
	    (lambda (xs x) (list xs (curried x)))
	    (curried t1)
	    ts))))

'+++
(curried '(a (b c d) (e f g h)))
(curried '(lambda (x y z) (x y (x y z))))
(curried '(if (a b c) (d e f) (cons h i)))

(defun (add a)
  (lambda (b)
    (+ a b)))

      (curried '((lambda (f x y) (f x y)) (lambda (a b) (add a b)) 3 4))
(eval (curried '((lambda (f x y) (f x y)) (lambda (a b) (add a b)) 3 4)))

      (curried '((lambda (f x y) (f x y)) (lambda (a b) (+ a b)) 3 4))
(eval (curried '((lambda (f x y) (f x y)) (lambda (a b) (+ a b)) 3 4)))
'---

; Patterned after https://matt.might.net/articles/cps-conversion/

(defun (cps-val val)
  (match val
	 (_ val when (atom val))

	 ((lambda (, arg) , body)
	  (let ((k (gensym)))
	    (` lambda (, arg , k)
	       , (cps-term body k))))
	 ))

(defun (cps-term term (k 'halt))
  (match term
	 (_ (list k (cps-val term)) when (atom term))

	 ((lambda (, arg) , body)
	  (list k (cps-val term)))

	 ((, t1 , t2)
	  (let ((fs (gensym))
		(as (gensym)))
	    (cps-term t1
	      (` lambda (, fs)
		 , (cps-term t2
		     (` lambda (, as) (, fs , as , k)))))))
	 ))

'+++
(cps-term (curried '(g a)))
'---

(defun (mkk k) (lambda (x) (list k x)))

(defun (cps-term term (k (mkk 'halt)))
  (match term
	 ((lambda (, arg) , body)
	  (let ((k' (gensym)))
	    (k (` lambda (, arg , k')
		  , (cps-term body (mkk k'))))))

	 (_ (k term) when (atom term))

	 ((, t1 , t2)
	  (let* ((rv (gensym))
		 (k' (` lambda (, rv) , (k rv))))
	    (cps-term t1
	      (lambda (fs)
		(cps-term t2
		  (lambda (as)
		    (` , fs , as , k')))))))
	 ))

'+++
(cps-term (curried '(a b c)))
'---

; (Own approach to hybrid translation - might be very flawed/buggy)

(defun (proc? p)
  (cond ((atom p) ())
	((atom (car p)) ())
	((eq (caar p) 'lambda) t)))

(defun (mk-k k)
  (if (proc? k) k
    (lambda (x) (list k x))))

(defun (gen-k k)
  (if (atom k) k
    (let ((s (gensym)))
      (` lambda (, s) , (k s)))))

(defun (cps-term term (k 'halt))
  (match term
	 ((lambda (, arg) , body)
	  (let ((k' (gensym)))
	    ((mk-k k) (` lambda (, arg , k')
		   , (cps-term body k')))))

	 (_ ((mk-k k) term) when (atom term))

	 ((, t1 , t2)
	  (let ((k' (gen-k k)))
	    (cps-term t1 (lambda (s1)
		(cps-term t2 (lambda (s2)
		    (` , s1 , s2 , k')))))))

	 ; ???
	 ((, op , t1 , t2)
	  (let ((k' (gen-k k)))
	    (cps-term t1 (lambda (s1)
	        (cps-term t2 (lambda (s2)
		    (` , op , s1 , s2 , k'))))))
	  when (member op binops))

	 ((if , t1 , t2 , t3)
	  (let ((k' (gen-k k))
		(ks (gensym)))
	    (cps-term t1 (lambda (s1)
		(` (lambda (, ks)
		     (if , s1
		       , (cps-term t2 ks)
		       , (cps-term t3 ks)))
		   , k')))))

	 (_ (list 'stuck term))
	 ))

'+++
(cps-term (curried '(a b c)))

(defun (halt x) x)

(defun (add a k)
  (k (lambda (b k')
       (k' (+ a b)))))

      (cps-term (curried '(add 1 2)))
(eval (cps-term (curried '(add 1 2))))

      (cps-term (curried '(add (add 1 2) (add 3 4))))
(eval (cps-term (curried '(add (add 1 2) (add 3 4)))))
      (cps-term (curried '(add (add 1 2) (add 3 (add 4 5)))))
(eval (cps-term (curried '(add (add 1 2) (add 3 (add 4 5))))))

(cps-term (curried '(eq (a b) c)))
(cps-term (curried '(if (eq (a b) (c d)) d e)))
(cps-term (curried '(func (if (eq (a b) (c d)) (d e) f))))
'---
