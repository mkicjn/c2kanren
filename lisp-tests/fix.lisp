; Experimenting with different fixpoint combinator implementations

; Y combinator test
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define last
  (Y (lambda (f)
       (lambda (ls) (cond ((atom ls) ls)
			  ((not (cdr ls)) (car ls))
			  ('else (f (cdr ls))))))))

(eq 'z (last '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; A version that doesn't rely on its own global definition
(define Z
  ((lambda (z) (z z))
   (lambda (z)
     (lambda (f)
       (f (lambda args (((z z) f) . args)))))))

(define last
  (Z (lambda (f)
       (lambda (ls) (cond ((atom ls) ls)
			  ((not (cdr ls)) (car ls))
			  ('else (f (cdr ls))))))))

(eq 'z (last '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; A version that uses currying to allow the self-referencing argument to be in the same lambda
(define curry (lambda (f x) (lambda args (f x . args))))
(define fix (lambda (f) (lambda args ((curry f (fix f)) . args))))

(define last
  (fix (lambda (f ls) (cond ((atom ls) ls)
			    ((not (cdr ls)) (car ls))
			    ('else (f (cdr ls)))))))

(eq 'z (last '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))


; Infinite recursion test
; Executing this should not exhaust the stack or cell space if everything is working properly
;((lambda (f) (f f)) (lambda (f) (f f)))
