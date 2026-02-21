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
	 ((iszero (succ , nv)) 'false when (is-nv nv))
	 ((iszero , t1) (` iszero , (-NB-> t1)))
	 (_ 'stuck)))

(define -NB->* (make->* -NB->))

; (tests)
'---
(is-nv 'zero)
(is-nv '(succ (succ zero)))

(equal (-NB->* '(if (iszero (pred (succ zero))) then (succ zero) else zero)) '(succ zero))


;; Untyped lambda calculus (λ)

; Numeric indexing of lists
(defun (nth n l)
  (if (> n 0) (nth (- n 1) (cdr l))
    (car l)))

(defun (position x l (acc 0))
  (cond ((not l) ())
	((eq x (car l)) acc)
	(t (position x (cdr l) (+ acc 1)))))

; Exercise 6.1.5
(defun (removenames t0 (Γ ()))
  (match t0
	 ((λ , x , t1) (` λ , (removenames t1 (cons x Γ))))
	 ((, t1 , t2) (` , (removenames t1 Γ) , (removenames t2 Γ)))
	 (_ (position t0 Γ) when (atom t0))))

(defun (shift d t0 (c 0))
  (match t0
	 ((λ , t1) (` λ , (shift d t1 (+ c 1))))
	 ((, t1 , t2) (` , (shift d t1 c) , (shift d t2 c)))
	 (_ t0 when (> c t0))
	 (_ (+ t0 d))))

(defun (sub j s k)
  (match k
	 ((λ , t1) (` λ , (sub (+ j 1) (shift 1 s) t1)))
	 ((, t1 , t2) (` , (sub j s t1) , (sub j s t2)))
	 (_ s when (eq j k))
	 (_ k)))

(defun (-λ-> t0) ; (small-step call by value semantics)
  (match t0
	 ((λ _) 'stuck)
	 (((λ , t12) (λ , t22))
	  (shift -1 (sub 0 (shift 1 (` λ , t22)) t12)))
	 (((λ , t12) , t2) (` (λ , t12) , (-λ-> t2)))
	 ((, t1 , t2) (` , (-λ-> t1) , t2))
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
	 (_ (nth t0 Γ) when (+ 0 t0))))

(restorenames '(λ (λ 0)))
(restorenames '(λ (λ (1 (1 0)))))
(restorenames '(λ (λ (λ (λ ((3 1) ((2 0) 1)))))))
(restorenames '(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0)))))))
(restorenames '((λ (λ 0)) (λ 0)))

; Exercise 6.2.2
'---
(shift 2 '(λ (λ (1 (0 2)))))
(shift 2 '(λ ((0 1) (λ ((0 1) 2)))))

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

(defun (test-I=L x)
  (let* ((x` (levels x))
	 (x`` (indices x`)))
    (list (equal x x``) x '\
	  '\  x`)))

(test-I=L '(λ (λ 0)))
(test-I=L '(λ (λ (1 (1 0)))))
(test-I=L '(λ (λ (λ (λ ((3 1) ((2 0) 1)))))))
(test-I=L '(λ ((λ (1 (λ ((1 1) 0)))) (λ (1 (λ ((1 1) 0)))))))
(test-I=L '((λ (λ 0)) (λ 0)))

; Evaluator
'---
(define c0 '(λ s (λ z z)))
(define c1 '(λ s (λ z (s z))))
(define c2 '(λ s (λ z (s (s z)))))
(define c3 '(λ s (λ z (s (s (s z))))))
(define c4 '(λ s (λ z (s (s (s (s z)))))))
(define scc '(λ n (λ s (λ z (s ((n s) z))))))
(define cplus '(λ m (λ n (λ s (λ z ((m s) ((n s) z)))))))
(define cmult '(λ m (λ n (λ s (m (n s))))))
(define cpow  '(λ m (λ n (n m))))

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

; Exercises 5.3.8, 7.3.1
'---
(defun (λ-norm t0)
  (match t0
	 (_ t when (atom t0))
	 (((λ _) _) ())
	 ((λ , t12) (λ-norm t12))
	 ((, t1 , t2) (and (λ-norm t1) (λ-norm t2)))))

(defun (λ⇓ t0) ; (big-step semantics)
  (match t0
	 (_ t0 when (λ-norm t0))
	 ((, t1 , t2) (λ⇓ (` , t1 , (λ⇓ t2))) when (not (λ-norm t2)))
	 ((, t1 , t2) (λ⇓ (` , (λ⇓ t1) , t2)) when (not (λ-norm t1)))
	 ((λ , t12) (` λ , (λ⇓ t12)) when (not (λ-norm t12)))
	 ; ^^^ Previous 3 can all be rearranged to change eval order
	 (((λ , t12) , t2) (λ⇓ (shift -1 (sub 0 (shift 1 t2) t12))))
	 (_ (` wrong , t0))))

(λ-norm (removenames '(λ x x)))
(λ-norm (removenames '(λ x (λ y (x y)))))
(λ-norm (removenames '(λ x (λ y (x (λ z (z z)))))))
(not (λ-norm (removenames '(λ x (λ y (x ((λ z (z z)) y)))))))

(equal (λ⇓ (removenames '(λ x (λ y (x ((λ z (z z)) y))))))
       (removenames '(λ x (λ y (x (y y))))))

; 2 + 2 = 4
(equal (λ⇓ (removenames (` (, cplus , c2) , c2)))
       (removenames c4))

(defun (show t0)
  (let ((t0` (removenames t0)))
    (list t0` '\
	  '→ (λ⇓ t0`))))

(show (` (, cplus , c2) , c2)) ; 2 + 2 = 4
(show (` (, cpow  , c2) , c3)) ; 2³ = 8
(show (` (, cpow , cplus) , cplus))

;; Experimental nameless term reducer (not from the book) - explicit substitution?
; TODO: Broken - but would be nice to get something like this working
'---

(defun (is-λ t0)
  (and (not (atom t0)) (eq 'λ (car t0))))

(defun (↓ t0 (Γ ()))
  (match t0
	 (_ (let ((b (nth t0 Γ)))
	      (if (eq b ()) t0 b))
	    when (atom t0))
	 ((λ , t1) (` λ , (↓ t1 (cons () Γ))))
	 (((λ , t12) , t2) (↓ t12 (cons t2 Γ)))
	 ((, t1 , t2)
	  (let ((t1` (↓ t1 Γ))
		(t2` (↓ t2 Γ)))
	    (let ((t0` (` , t1` , t2`)))
	      (if (is-λ t1`) (↓ t0` Γ) t0`))))
	 (_ 'wrong)))


(↓ (removenames (` (, cplus , c0) , c4)))
(↓ (removenames (` (, cplus , c1) , c3)))
(↓ (removenames (` (, cplus , c2) , c2)))
(↓ (removenames (` (, cplus , c3) , c1)))

(eq (λ⇓ (removenames (` (, cmult , c2) , c3)))
    (↓ (removenames (` (, cmult , c2) , c3))))


;; Chapter 8 - typed arithmetic expressions
'---

(defun (NB-type t0)
  (match t0
	 (false 'Bool)
	 (true  'Bool)
	 (zero  'Nat)
	 ((succ , n) 'Nat when (eq (NB-type n) 'Nat))
	 ((pred , n) 'Nat when (eq (NB-type n) 'Nat))
	 ((iszero , n) 'Bool when (eq (NB-type n) 'Nat))
	 ((if , t1 then , t2 else , t3)
	  (NB-type t2)
	  ; ^ TODO: Redundant calculation from guard, but unclear how to avoid
	  when (and (eq (NB-type t1) 'Bool)
		    (eq (NB-type t2) (NB-type t3))))))

(eq 'Nat (NB-type '(if (iszero (succ (pred zero))) then zero else (succ zero))))
(eq 'Bool (NB-type '(iszero (if (iszero (succ (pred zero))) then zero else (succ zero)))))
(eq '() (NB-type '(pred (iszero (if (iszero (succ (pred zero))) then zero else (succ zero))))))


;; Chapter 9 - simply typed lambda calculus
'---

(defun (λ→-erase t0)
  (match t0
	 ((λ , x : _ , t1) (` λ , x , (λ→-erase t1)))
	 ((, t1 , t2) (` , (λ→-erase t1) , (λ→-erase t2)))
	 (_ t0)))

; TODO: Implement type analyses for the resulting language

(defun (removenames t0 (Γ ()))
  (match t0
	 ((λ , x , t1) (` λ , (removenames t1 (cons x Γ))))
	 ((, t1 , t2) (` , (removenames t1 Γ) , (removenames t2 Γ)))
	 (_ (map (lambda (tn) (removenames tn Γ)) t0) when (not (atom t0)))
	 (_ (let ((pos (position t0 Γ)))
	      (if pos pos t0)))))

(defun (shift d t0 (c 0))
  (match t0
	 ((λ , t1) (` λ , (shift d t1 (+ c 1))))
	 ((, t1 , t2) (` , (shift d t1 c) , (shift d t2 c)))
	 (_ (map (lambda (tn) (shift d tn c)) t0) when (not (atom t0)))
	 (_ t0 when (> c t0))
	 (_ (+ t0 d) when (+ 0 t0))
	 (_ t0)))

(defun (sub j s t0)
  (match t0
	 ((λ , t1) (` λ , (sub (+ j 1) (shift 1 s) t1)))
	 ((, t1 , t2) (` , (sub j s t1) , (sub j s t2)))
	 (_ (map (lambda (tn) (sub j s tn)) t0) when (not (atom t0)))
	 (_ s when (eq j t0))
	 (_ t0)))

(defun (-λ→-> t0) ; (small-step call by value semantics)
  ; Use removenames and λ→-erase on t0
  (match t0
	 ((if true then , t2 else _) t2)
	 ((if false then _ else , t3) t3)
	 ((if , t1 then , t2 else , t3) (` if , (-λ→-> t1) then , t2 else , t3))
	 ((succ , t1) (` succ , (-λ→-> t1)) when (not (is-nv t1)))
	 ((succ _) 'stuck)
	 ((pred zero) 'zero)
	 ((pred (succ , nv)) nv when (is-nv nv))
	 ((pred , t1) (` pred , (-λ→-> t1)))
	 ((iszero zero) 'true)
	 ((iszero (succ , nv)) 'false when (is-nv nv))
	 ((iszero , t1) (` iszero , (-λ→-> t1)))
	 ((λ _) 'stuck)
	 (((λ , t1) , t2)
	  (let ((t2` (-λ→-> t2)))
	    (if (eq t2` 'stuck)
	      (shift -1 (sub 0 (shift 1 t2) t1))
	      (` (λ , t1) , t2`))))
;	  (if (is-λ t2)
;	    (shift -1 (sub 0 (shift 1 t2) t1))
;	    (` (λ , t1) , (-λ→-> t2))))
	 ((, t1 , t2) (` , (-λ→-> t1) , t2))
	 (_ 'stuck)))

(defun (λ→-type t0 (Γ ()))
  (match t0
	 (false 'Bool)
	 (true  'Bool)
	 (zero  'Nat)
	 ((succ , n) 'Nat when (eq (λ→-type n Γ) 'Nat))
	 ((pred , n) 'Nat when (eq (λ→-type n Γ) 'Nat))
	 ((iszero , n) 'Bool when (eq (λ→-type n Γ) 'Nat))
	 ((if , t1 then , t2 else , t3)
	  (λ→-type t2 Γ)
	  ; ^ TODO: Redundant calculation from guard, but unclear how to avoid
	  when (and (eq (λ→-type t1 Γ) 'Bool)
		    (equal (λ→-type t2 Γ) (λ→-type t3 Γ))))
	 ((λ , x : , T1 , t1) ; T-Abs
	  (let ((T2 (λ→-type t1 (cons (cons x T1) Γ))))
	    (if T2 (` , T1 -> , T2))))
	 ((, t1 , t2) ; T-App
	  (let ((T1 (λ→-type t1 Γ))
		(T2 (λ→-type t2 Γ)))
	    (match T1
		   ((, T11 -> , T12) T12 when (equal T2 T11)))))
	 (_ ; T-Var
	   (cdr (assoc t0 Γ)) when (atom t0))))

; (tests)

; These are for testing purposes and won't typecheck
(define C0 '(λ s : T (λ z : T z)))
(define C1 '(λ s : T (λ z : T (s z))))
(define C2 '(λ s : T (λ z : T (s (s z)))))
(define C3 '(λ s : T (λ z : T (s (s (s z))))))
(define C4 '(λ s : T (λ z : T (s (s (s (s z)))))))
(define Scc '(λ n : T (λ s : T (λ z : T (s ((n s) z))))))
(define Cplus '(λ m : T (λ n : T (λ s : T (λ z : T ((m s) ((n s) z)))))))
(define Cmult '(λ m : T (λ n : T (λ s : T (m (n s))))))
(define Cpow  '(λ m : T (λ n : T (n m))))

(equal (λ→-erase Cplus) cplus)

(equal (times 0 -λ→-> (removenames (λ→-erase (` (, Cplus , C2) , C1))))
       '(((λ (λ (λ (λ ((3 1) ((2 1) 0)))))) (λ (λ (1 (1 0))))) (λ (λ (1 0)))))
(equal (times 1 -λ→-> (removenames (λ→-erase (` (, Cplus , C2) , C1))))
       '((λ (λ (λ (((λ (λ (1 (1 0)))) 1) ((2 1) 0))))) (λ (λ (1 0)))))
(equal (times 2 -λ→-> (removenames (λ→-erase (` (, Cplus , C2) , C1))))
       '(λ (λ (((λ (λ (1 (1 0)))) 1) (((λ (λ (1 0))) 1) 0)))))
(equal (times 3 -λ→-> (removenames (λ→-erase (` (, Cplus , C2) , C1))))
       'stuck)


(equal (times 0 -λ→-> (removenames '(if (iszero (pred (succ zero))) then false else true)))
       '(if (iszero (pred (succ zero))) then false else true))
(equal (times 1 -λ→-> (removenames '(if (iszero (pred (succ zero))) then false else true)))
       '(if (iszero zero) then false else true))
(equal (times 2 -λ→-> (removenames '(if (iszero (pred (succ zero))) then false else true)))
       '(if true then false else true))
(equal (times 3 -λ→-> (removenames '(if (iszero (pred (succ zero))) then false else true)))
       'false)

(eq 'Bool (λ→-type '(if (iszero (pred (succ zero))) then false else true)))
(eq () (λ→-type '(if (iszero (pred (succ zero))) then false else zero))) ; Ill-typed
(eq 'Nat (λ→-type '(if (iszero (pred (succ zero))) then (succ zero) else zero)))

(equal '(Nat -> Bool) (λ→-type '(λ x : Nat (iszero x))))
(equal '(Nat -> Nat) (λ→-type '(λ x : Nat (if (iszero x) then (succ zero) else zero))))
(equal '((Nat -> Bool) -> (Nat -> Nat))
       (λ→-type '(λ f : (Nat -> Bool) (λ x : Nat (if (f x) then (succ x) else (pred x))))))

(let* ((t0 '(((λ f : (Nat -> Bool)
		 (λ x : Nat (if (f x) then (succ x) else (pred x))))
	      (λ y : Nat (iszero y)))
	     (succ (succ zero))))
       (t0` (removenames (λ→-erase t0))))
  (list (λ→-type t0) '\
	(times 0 -λ→-> t0`) '\
	(times 1 -λ→-> t0`) '\
	(times 2 -λ→-> t0`) '\
	(times 3 -λ→-> t0`) '\
	(times 4 -λ→-> t0`) '\
	(times 5 -λ→-> t0`) '\
	(times 6 -λ→-> t0`)))
