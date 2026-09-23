; Bracket Abstraction Algorithms
; https://www.cantab.net/users/antoni.diller/brackets/intro.html

(defun (bracket-ski term x)
  (if (atom term)
    (if (eq term x) 'I (list 'K term))
    (list 'S (bracket-ski (car term) x)
             (bracket-ski (cadr term) x))))

(bracket-ski '((((u v) ((w z) x)) ((x z) y)) ((z x) (y x))) 'x)

(defun (contains l x)
  (cond ((eq l x) t)
	((atom l) ())
	((contains (car l) x) t)
	((contains (cdr l) x) t)
	(t ())))

(defun (bracket-skibc term x)
  (if (atom term)
    (if (eq term x) 'I (list 'K term))
    (let ((E1 (not (contains (car term) x)))
	  (E2 (not (contains (cadr term) x))))
      (cond ((and E1 E2) (list 'K term))
	    ((not (or E1 E2)) (list 'S (bracket-skibc (car term) x)
				       (bracket-skibc (cadr term) x)))
	    ((if E1 (eq (cadr term) x)) (car term))
	    (E1 (list 'B (car term) (bracket-skibc (cadr term) x)))
	    (E2 (list 'C (bracket-skibc (car term) x) (cadr term)))))))

(bracket-skibc '((((u v) ((w z) x)) ((x z) y)) ((z x) (y x))) 'x)
