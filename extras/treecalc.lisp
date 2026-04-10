; Tree calculus evaluator

; Adapted from https://olydis.medium.com/a-visual-introduction-to-tree-calculus-2f4a34ceffc2
(defun (app tree z)
  (match tree
	 ((Δ) (` Δ , z))
	 ((Δ , y) (` Δ , y , z))
	 ((Δ (Δ) , y) y)
	 ((Δ (Δ , x) , y) (app (app x z) (app y z)))
	 ((Δ (Δ , w , x) , y)
	  (match z
		 ((Δ) w)
		 ((Δ , u) (app x u))
		 ((Δ , u , v) (app (y u) v))))))

; Adapted from example on https://treecalcul.us/specification/
(define t-false '(Δ))
(define t-true '(Δ (Δ)))
(define t-not '(Δ (Δ (Δ (Δ)) (Δ (Δ) (Δ))) (Δ)))

(app t-not t-true) ; (Δ)
(app t-not t-false) ; (Δ (Δ))


; Parentheses-notation (much easier to read than the example, I think)
; Literally s/Δ ?//g
(defun (app tree z)
  (match tree
	 (() (` , z))
	 ((, y) (` , y , z))
	 ((() , y) y)
	 (((, x) , y) (app (app x z) (app y z)))
	 (((, w , x) , y)
	  (match z
		 (() w)
		 ((, u) (app x u))
		 ((, u , v) (app (y u) v))))))

(define t-false '())
(define t-true '(()))
(define t-not '(((()) (() ())) ()))

(app t-not t-true) ; ()
(app t-not t-false) ; (())
