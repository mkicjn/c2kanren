
(defun (zip xs ys (end ()))
  (cond ((atom xs) end)
	((atom ys) end)
	(t (cons (cons (car xs) (car ys))
		 (zip  (cdr xs) (cdr ys) end)))))

; Evaluator example using `match`
(defun (ev (env ()))
  (lambda (form)
    (match form
      ; Self-evaluating symbols
      (t t)
      (() ())
      ; Variable lookup
      (_ (cdr (assoc form env)) when (atom form))
      ; Primitives / special forms
      ((quote , x) x)
      ((if , con , then , else)
       (if ((ev env) con)
         ((ev env) then)
         ((ev env) else)))
      ((cons , a , b)
       (cons ((ev env) a) ((ev env) b)))
      ((car , a)
       (car ((ev env) a)))
      ((cdr , a)
       (cdr ((ev env) a)))
      ((eq , a , b)
       (eq ((ev env) a) ((ev env) b)))
      ; Closures & Application
      ((lambda , vars , body)
       (` closure , env , vars , body))
      ((, func ,. args)
       (match ((ev env) func)
	 ((closure , env` , vars , body)
	  ((ev (zip vars (map (ev env) args) env`)) body))
	 (_ 'error))))))

((ev) '((lambda (x) (eq x x)) (cons 'a 'b)))

((ev) '((lambda (f x) (f x x)) (lambda (x y) (eq x y)) (lambda (x) (x x))))

; (infinite recursion)
((ev) '((lambda (x) (x x)) (lambda (x) (x x))))
