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
    (if (eq term x) 'I (` K , term))
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

(bracket skibc '(λ s (λ z (s (s z)))))
(bracket skibc '(λ s (λ z (s (s (s z))))))
(bracket skibc '(λ s (λ z (s (s (s (s z)))))))

(bracket ski '(λ x (x x)))

(bracket skibc '(λ x (λ y ((x y) y))))

; (λpow. λc2. λc3. pow c2 c3) (λm.λn. n m) (λs.λz. s (s z)) (λs.λz. s (s (s z)))
(bracket skibc '((((λ pow (λ c2 (λ c3 ((pow c2) c3)))) (λ m (λ n (n m)))) (λ s (λ z (s (s z))))) (λ s (λ z (s (s (s (s z))))))))
(bracket skibc '(λ n (λ s (λ z (s (n z))))))
(bracket skibc '(λ s (λ z z)))
