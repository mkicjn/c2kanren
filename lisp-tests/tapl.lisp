; Experimental implementation of languages from TAPL (Pierce)
; Run with, e.g., `../c2klisp.c ../rc.lisp tapl.lisp -`

; (Note that the arrow names are technically a slight misnomer,
;  since they're functions instead of relations on the set of terms)

(defun (make->* ->)
  (Z (lambda (->*)
       (lambda (t0)
	 (let ((t0` (-> t0)))
	   (if (eq t0` 'stuck) t0 (->* t0`)))))))


;; Bool language (B)

(defun (-B-> t0)
  (match t0
	 ((if true then , t2 else _) t2)
	 ((if false then _ else , t3) t3)
	 ((if , t1 then , t2 else , t3) (` if , (-B-> t1) then , t2 else , t3))
	 (_ 'stuck)))

(define -B->* (make->* -B->))

; (tests)
(eq (-B->* '(if true then (if (if false then false else true) then false else true) else false))
    'false)


;; Nat language (NB - extends B)

(defun (is-nv t0)
  (match t0
	 (zero t)
	 ((succ , t1) (is-nv t1))))

(defun (-NB-> t0)
  (match t0
	 ((if true then , t2 else _) t2)
	 ((if false then _ else , t3) t3)
	 ((if , t1 then , t2 else , t3) (` if , (-NB-> t1) then , t2 else , t3))
	 ((succ , t1) (` succ , (-NB-> t1)) when (not (is-nv t1)))
	 ((pred zero) 'zero)
	 ((pred (succ , nv)) nv when (is-nv nv))
	 ((pred , t1) (` pred , (-NB-> t1)))
	 ((iszero zero) 'true)
	 ((iszero (succ nv)) 'false when (is-nv nv))
	 ((iszero , t1) (` iszero , (-NB-> t1)))
	 (_ 'stuck)))

(define -NB->* (make->* -NB->))

; (tests)
(is-nv 'zero)
(is-nv '(succ (succ zero)))

(equal (-NB->* '(if (iszero (pred (succ zero))) then (succ zero) else zero)) '(succ zero))


;; Untyped lambda calculus (λ)

; Exercise 6.1.5
(defun (removenames t0 (Γ ()))
  (match t0
	 ((λ , x , t1) (` λ , (removenames t1 (cons x Γ))))
	 ((, t1 , t2) (` , (removenames t1 Γ) , (removenames t2 Γ)))
	 (_ (position t0 Γ) when (atom t0))))

(defun (restorenames t0 (Γ ()))
  (match t0
	 ((λ , t1) (let ((x (gensym)))
			(` λ , x , (restorenames t1 (cons x Γ)))))
	 ((, t1 , t2) (` , (restorenames t1 Γ) , (restorenames t2 Γ)))
	 (_ (nth t0 Γ) when (eq (type t0) 'number))))


; (tests)
'(λ s (λ z z))
'(λ s (λ z (s (s z))))
'(λ m (λ n (λ s (λ z ((m s) ((n z) s))))))
'(λ f ((λ x (f (λ y ((x x) y)))) (λ x (f (λ y ((x x) y))))))
'((λ x (λ x x)) (λ x x))

(removenames '(λ s (λ z z)))
(removenames '(λ s (λ z (s (s z)))))
(removenames '(λ m (λ n (λ s (λ z ((m s) ((n z) s)))))))
(removenames '(λ f ((λ x (f (λ y ((x x) y)))) (λ x (f (λ y ((x x) y)))))))
(removenames '((λ x (λ x x)) (λ x x)))

(restorenames '(λ (λ 0)))
(restorenames '(λ (λ (1 (1 0)))))
(restorenames '(λ (λ (λ (λ ((3 1) ((2 0) 1)))))))
(restorenames '(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0)))))))
(restorenames '((λ (λ 0)) (λ 0)))
