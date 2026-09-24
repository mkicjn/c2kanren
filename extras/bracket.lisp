; Bracket Abstraction Algorithms
; https://www.cantab.net/users/antoni.diller/brackets/intro.html

(defun (ski term x)
  (if (atom term)
    (if (eq term x) 'I (` K , term))
    (` (S , (ski (car term) x))
       , (ski (cadr term) x))))

(ski '((((u v) ((w z) x)) ((x z) y)) ((z x) (y x))) 'x)

(defun (contains l x)
  (cond ((eq l x) t)
	((atom l) ())
	((contains (car l) x) t)
	((contains (cdr l) x) t)
	(t ())))

(defun (skibc term x)
  (if (atom term)
    (if (eq term x) 'I (` 'K term))
    (let ((E1 (not (contains (car term) x)))
	  (E2 (not (contains (cadr term) x))))
      (cond ((and E1 E2) (` K , term))
	    ((not (or E1 E2)) (` (S , (skibc (car term) x))
				 , (skibc (cadr term) x)))
	    ((if E1 (eq (cadr term) x)) (car term))
	    (E1 (` (B , (car term)) , (skibc (cadr term) x)))
	    (E2 (` (C , (skibc (car term) x)) , (cadr term)))))))

(skibc '((((u v) ((x z) w)) ((w z) y)) ((z w) (y w))) 'x)

(defun (bracket f term)
  (match term
    ((λ , v , b) (f (bracket f b) v))
    ((, x , y) (` , (bracket f x) , (bracket f y)))
    (_ term)))

(bracket skibc '(λ z (λ y (λ w (λ v (λ u (λ x ((((u v) ((w z) x)) ((x z) y)) ((z x) (y x))))))))))

(bracket skibc '(λ f ((λ g (f (g g))) (λ g (f (g g))))))
