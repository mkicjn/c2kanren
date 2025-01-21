; Implementing destructuring bind

(defun (sym-locs l cont)
  (cond ((not l) ())
	((atom l) (list (list l (cont))))
	(t (append (sym-locs (car l) (lambda () (list 'car (cont))))
		   (sym-locs (cdr l) (lambda () (list 'cdr (cont))))))))

(sym-locs '(a (b c)) (lambda () 'ls))


(defmacro (destructure vars vals body)
  (` let ((_ , vals) ,@ (sym-locs vars (lambda () '_))) , body))


(expand '(destructure (a (b c)) (cons 'x (cons (cons 'y (cons 'z ())) ()))
		      (cons a b)))

(destructure (a (b c)) (cons 'x (cons (cons 'y (cons 'z ())) ()))
		      (cons a b))
