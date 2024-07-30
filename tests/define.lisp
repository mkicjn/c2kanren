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
(ident a)
