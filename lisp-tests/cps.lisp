; Comparison of naive recursion vs. continuation passing style
; (This is more interesting when compiling with -DDEBUG)

(define fib-gen
  (let ((a 0) (b 1))
    (lambda (a b)
      (lambda () (cons b (fib-gen b (+ a b)))))))

(define take-naive
  (lambda (n s)
    (cond ((= n 0) ())
	  ((eq (type s) 'lambda) (take-naive n (s)))
	  ((atom s) s)
	  (t (cons (car s) (take-naive (- n 1) (cdr s)))))))

(take-naive 93 (fib-gen))


; Same with continuation passing style

(define take-cps
  (let ((cont (lambda (x) x)))
    (lambda (n s cont)
      (cond ((= n 0) (cont ()))
	    ((eq (type s) 'lambda) (take-cps n (s) cont))
	    ((atom s) (cont s))
	    (t (take-cps (- n 1) (cdr s)
			 (lambda (x) (cont (cons (car s) x)))))))))

(take-cps 93 (fib-gen))
