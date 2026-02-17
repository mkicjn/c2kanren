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
'---
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
'---
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

(defun (shift t0 d (c 0))
  (match t0
	 ((λ , t1) (` λ , (shift t1 d (+ c 1))))
	 ((, t1 , t2) (` , (shift t1 d c) , (shift t2 d c)))
	 (_ t0 when (> c t0))
	 (_ (+ t0 d))))

(defun (sub j s k)
  (match k
	 ((λ , t1) (` λ , (sub (+ j 1) (shift s 1) t1)))
	 ((, t1 , t2) (` , (sub j s t1) , (sub j s t2)))
	 (_ s when (eq j k))
	 (_ k)))

(defun (-λ-> t0) ; (single step call by value semantics)
  (match t0
	 ((λ _) 'stuck)
	 (((λ , t12) (λ , t22))
	  (shift (sub 0 (shift (` λ , t22) 1) t12) -1))
	 (((λ , t12) , t2)
	  (` (λ , t12) , (-λ-> t2)))
	 ((, t1 , t2)
	  (` , (-λ-> t1) , t2))
	 (_ 'stuck)))

(define -λ->* (make->* -λ->))

; (tests)

; Exercise 6.1.1
'---
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

(defun (restorenames t0 (Γ ()))
  (match t0
	 ((λ , t1) (let ((x (gensym)))
			(` λ , x , (restorenames t1 (cons x Γ)))))
	 ((, t1 , t2) (` , (restorenames t1 Γ) , (restorenames t2 Γ)))
	 (_ (nth t0 Γ) when (eq (type t0) 'number))))

(restorenames '(λ (λ 0)))
(restorenames '(λ (λ (1 (1 0)))))
(restorenames '(λ (λ (λ (λ ((3 1) ((2 0) 1)))))))
(restorenames '(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0)))))))
(restorenames '((λ (λ 0)) (λ 0)))

; Exercise 6.2.2
'---
(shift '(λ (λ (1 (0 2)))) 2)
(shift '(λ ((0 1) (λ ((0 1) 2)))) 2)

; Exercise 6.2.5
'---
(defun (Γ-sub Γ . args)
  (let ((args` (map (lambda (x) (removenames x Γ)) args)))
    (sub . args`)))
(Γ-sub '(b a) 'b 'a '(b (λ x (λ y b))))
(Γ-sub '(b a) 'b '(a (λ z a)) '(b (λ x b)))
(Γ-sub '(b a) 'b 'a '(λ b (b a)))
(Γ-sub '(b a) 'b 'a '(λ a (b a)))

; Exercise 6.3.2
'---
(defun (levels t0 (lvl -1))
  (match t0
	 ((λ , t1) (` λ , (levels t1 (+ lvl 1))))
	 ((, t1 , t2) (` , (levels t1 lvl) , (levels t2 lvl)))
	 (_ (- lvl t0))))
(define indices levels) ; !!

(levels '(λ ((λ (1 0)) 0)))
(indices '(λ ((λ (0 1)) 0)))

(defun (test x)
  (let* ((x` (levels x))
	 (x`` (indices x`)))
    (list (equal x x``) x '\
	  '\  x`)))

(test '(λ (λ 0)))
(test '(λ (λ (1 (1 0)))))
(test '(λ (λ (λ (λ ((3 1) ((2 0) 1)))))))
(test '(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0)))))))
(test '((λ (λ 0)) (λ 0)))

; Evaluator
'---
(define c0 '(λ s (λ z z)))
(define c1 '(λ s (λ z (s z))))
(define c2 '(λ s (λ z (s (s z)))))
(define scc '(λ n (λ s (λ z (s ((n s) z))))))
(define cplus '(λ m (λ n (λ s (λ z ((m s) ((n s) z)))))))

(defun (times n f x)
  (if (> n 0)
    (times (- n 1) f (f x))
    x))

(equal (times 0 -λ-> (removenames (` (, cplus , c2) , c1)))
       '(((λ (λ (λ (λ ((3 1) ((2 1) 0)))))) (λ (λ (1 (1 0))))) (λ (λ (1 0)))))
(equal (times 1 -λ-> (removenames (` (, cplus , c2) , c1)))
       '((λ (λ (λ (((λ (λ (1 (1 0)))) 1) ((2 1) 0))))) (λ (λ (1 0)))))
(equal (times 2 -λ-> (removenames (` (, cplus , c2) , c1)))
       '(λ (λ (((λ (λ (1 (1 0)))) 1) (((λ (λ (1 0))) 1) 0)))))
(equal (times 3 -λ-> (removenames (` (, cplus , c2) , c1)))
       'stuck)
