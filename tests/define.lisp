; Testing behavior of define as a primitive function

; (Trying to trigger a GC-related bug from the first attempt)
(define a (cons 'a 'b))
a
(define a (cons 'b 'c))
a

; (Testing the intended purpose)
(define list (lambda args args))
(define defun (macro (name args body)
		     (list define name
			   (list lambda args body))))
(defun ident (x) x)
(ident t)


; Scheme-like curried definitions
(define list (lambda args args))
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))
(define define+
  (macro (args body)
	 (cons define
	       ((Y (lambda (expand)
		     (lambda (args body)
		       (cond ((not args) body)
			     ((atom args) (list args body))
			     (t (expand (car args) (list lambda (cdr args) body)))))))
		args body))))

(define+ (((a b c) d) e) (+ b c d e))
(= 10 (((a 1 2) 3) 4))

(define+ a 'x)
(eq 'x a)
