; Executing this should not exhaust the stack or cell space if everything is working properly

; Y combinator test
(define Y (lambda (f) (f (lambda args ((Y f) . args)))))

(define last
  (Y (lambda (f)
       (lambda (ls) (cond ((atom ls) ls)
			  ((not (cdr ls)) (car ls))
			  ('else (f (cdr ls))))))))

(eq 'z (last '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; Infinite recursion test
;((Y (lambda (f) (lambda () (f)))))
