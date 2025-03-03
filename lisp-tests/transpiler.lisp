; Useful definitions

(defun (listify x)
  (cond ((atom x) (list x))
	(t x)))

(defun (join s x)
  (cond ((cdr x) (` ,@ (listify (car x)) , s ,. (join s (cdr x))))
	(x (listify (car x)))))

(defun (in s l)
  (cond ((not l) ())
	((eq s (car l)) t)
	(t (in s (cdr l)))))


;; Functions for getting info from a define or lambda expression

(defun (func-name def/lambda)
  (cond ((atom def/lambda) ())
	((eq (car def/lambda) 'lambda) '?)
	((eq (car def/lambda) 'define) (cadr def/lambda))
	(t ())))

(defun (func-lambda def/lambda)
  (cond ((atom def/lambda) ())
	((eq (car def/lambda) 'lambda) def/lambda)
	((eq (car def/lambda) 'define) (caddr def/lambda))
	(t ())))

(defun (func-args def/lambda)
  (cadr (func-lambda def/lambda)))

(defun (func-body def/lambda)
  (caddr (func-lambda def/lambda)))


;; Extract all functions from an expression

(defun (extract-funcs expr)
  (let ((name (func-name expr)))
    (cond (name (cons (cons (func-lambda expr) name)
		      (extract-funcs (func-body expr))))
	  ((atom expr) ())
	  (t (append (extract-funcs (car expr))
		     (extract-funcs (cdr expr)))))))


; Testing on some code that uses functions in various ways

(define append-sample '(

(define ident (lambda (x) x))
(define append_cps
  (lambda (cont l1 l2)
    (cond ((not l1) (cont l2))
	  (t (append_cps (lambda (x) (cont (cons (car l1) x)))
		      (cdr l1) l2)))))
(define append (lambda (l1 l2) (append_cps ident l1 l2)))


))

;(extract-funcs append-sample)

;; Assign valid identifier names to unnamed functions

(defun (rename-lambdas funcs names)
  (cond ((not funcs) ())
	((eq (cdar funcs) '?) (cons (cons (caar funcs) (car names))
				    (rename-lambdas (cdr funcs) (cdr names))))
	(t (cons (car funcs) (rename-lambdas (cdr funcs) names)))))


; Testing on the same sample as before

(define anon-names '(f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10))

;(rename-lambdas (extract-funcs append-sample) anon-names)


;; Extract all free values from a function

(defun (in x xs)
  (cond ((not xs) ())
	((eq x (car xs)) t)
	(t (in x (cdr xs)))))

(defun (free-vars-acc args expr acc)
  (cond ((not expr) acc)
	((atom expr) (cond ((in expr args) acc)
			   ((in expr acc) acc)
			   (t (cons expr acc))))
	((eq (car expr) 'quote) acc)
	((eq (car expr) 'lambda)
	    (free-vars-acc (append (func-args expr) args)
			   (func-body expr) acc))
	(t (free-vars-acc args (car expr)
		      (free-vars-acc args (cdr expr) acc)))))

(defun (free-vars args expr) (free-vars-acc args expr ()))


; Testing free variable extraction

(define primitives '(car cdr cons t atom eq cond not))

(let* ((funcs (rename-lambdas (extract-funcs append-sample) anon-names))
       (func-names (map (lambda (x) (cdr x)) funcs))
       (all-names (append primitives func-names))
       (free (map (lambda (x) (list (cdr x) (free-vars all-names (car x)))) funcs)))
  free)


;; Transpile function contents

(defun (exprs-to-lambdas exprs)
  (let* ((funcs (rename-lambdas (extract-funcs exprs) anon-names))
	 (func-names (map (lambda (x) (cdr x)) funcs))
	 (all-names (append primitives func-names)))
    (map (lambda (x) (list (car x) (cdr x) (free-vars all-names (car x)))) funcs)))

(defun (func-expr-to-name lambdas expr)
  (cadr (assoc expr lambdas)))

(defun (func-expr-to-freevars lambdas expr)
  (caddr (assoc expr lambdas)))

(defun (func-expr-to-args lambdas expr)
  (append (func-args expr) (func-expr-to-freevars lambdas expr)))

(defun (transpile-cond ls conds)
  (cond ((not conds) 'NULL)
	((eq (caar conds) t) (transpile-expr ls (cadar conds)))
	(t (list (transpile-expr ls (caar conds)) '? (transpile-expr ls (cadar conds))
		 ': (transpile-cond ls (cdr conds))))))

(defun (transpile-expr lambdas x)
  (cond ((not x) 'NULL)
	((eq x t) 'sym_t)
	((atom x) (cond ((in x (map (lambda (x) (cadr x)) lambdas)) (` CLOSURE (, x))) (t x)))
	((eq (car x) 'eq) (` ,@ (transpile-expr lambdas (cadr x)) == ,@ (transpile-expr lambdas (caddr x))))
	((eq (car x) 'quote) (` quote (" , (cadr x) ")))
	((eq (car x) 'lambda) (` CLOSURE (, (func-expr-to-name lambdas x))))
	((eq (car x) 'cond) (transpile-cond lambdas (cdr x)))
	((eq (car x) 'not) (` ! , (transpile-expr lambdas (cadr x))))
	((in (car x) '(car cdr cons))
	 (` , (car x) , (join ', (map (curry transpile-expr lambdas) (cdr x)))))
	((in (car x) (map (lambda (x) (cadr x)) lambdas))
	 (` , (car x) , (join ', (map (curry transpile-expr lambdas) (cdr x)))))
	(t (` CALL , (join ', (append (list (transpile-expr lambdas (car x))) (map (curry transpile-expr lambdas) (cdr x))))))))

(define nl '\
)

(define tab '\	)

(defun (transpile-lambdas0 ls0 ls)
  (cond ((not ls) ())
	(t (let ((l (caar ls)) (name (cadar ls)))
	     (append (` void * , name , (join ', (map (lambda (x) (` void * , x)) (func-expr-to-args ls0 l))) , nl
			{ , nl
			, tab return ,@ (listify (transpile-expr ls0 (caddr l))) , '\; , nl
			} , nl)
		     (transpile-lambdas0 ls0 (cdr ls)))))))

(defun (transpile-lambdas ls) (transpile-lambdas0 ls ls))

 
; Testing on the same sample as before

(exprs-to-lambdas append-sample)

(transpile-lambdas (exprs-to-lambdas append-sample))
; ^ The code returned by the above expression will compile and work from C with the following additions:
; TODO: Continue exploring closure conversion and transpiling Lisp to C
;
;#define list3(x, y, z) cons(x, list2(y, z))
;void *ident(void *x);
;void *closed_ident(void *self, void *x);
;void *f0(void *cont, void *l1, void *x);
;void *closed_f0(void *self, void *x);
;void *append_cps(void *cont, void *l1, void *l2);
;void *append(void *l1, void *l2);
;
;#define FOREACH_PRIM(X) \
;       X(ident) \
;       X(closed_ident) \
;       X(f0) \
;       X(closed_f0) \
;       X(append_cps) \
;       X(append)
;
;#define DECL_ENUM(F) F##_e,
;enum prim_e { FOREACH_PRIM(DECL_ENUM) };
;
;#define LIST_FUNC(F) F,
;void *(*prims[])() = { FOREACH_PRIM(LIST_FUNC) };
;
;#define CLOSURE(F) CLOSURE_##F
;#define FUNCTION(F) &prims[F##_e]
;
;static inline void *call1(void *x, void *a)
;{
;       void *(*f)(void *, void *) = *(void *(**)())*CAR(x);
;       return f(*CDR(x), a);
;}
;
;
;void *closed_ident(void *env, void *x)
;{
;       (void)env;
;       return ident(x);
;}
;#define CLOSURE_ident (list1(FUNCTION(closed_ident)))
;
;void *closed_f0(void *env, void *x)
;{
;       return f0(x, *CAR(env), *CAR(*CDR(env)));
;}
;#define CLOSURE_f0 (list3(FUNCTION(closed_f0), cont, l1))
;
;#define CALL call1



;; Messing around with an SSA-like representation

(defun (ssa l)
  (cond ((atom l) (list (list (gensym) l)))
	((eq (car l) 'quote) (list (list (gensym) l)))
	(t (cons (list (gensym) l)
		 (flatten (map ssa l))))))

(defun (get-match e ls)
  (assp (lambda (p) (equal e (cadr p))) ls))

(defun (dedup ls)
  (cond ((atom ls) ls)
	(t (let* ((lsd (dedup (cdr ls)))
		  (match (get-match (cadar ls) lsd)))
	     (cond (match lsd)
		   (t (cons (car ls) lsd)))))))


(defun (sub l ls)
  (or (car (get-match l ls)) l))

(defun (subs l ls)
  (cond ((atom l) l)
	(t (map (lambda (x) (sub x ls)) l))))

(defun (do-subs ls)
  (cond ((atom ls) ls)
	(t (cons (list (caar ls) (subs (cadar ls) ls))
		 (do-subs (cdr ls))))))

(defun (formatted ls)
  (cond ((atom ls) ls)
	(t (` void * , (caar ls) = , (cadar ls) , '\; , '\
	      ,@ (formatted (cdr ls))))))


;; Testing

(ssa '(cons 'a 'b))

(dedup (ssa '(cons 'a 'a)))

(subs '(cons 'a 'a) (dedup (ssa '(cons 'a 'a))))

;(do-subs (dedup (ssa '(cons 'a 'a))))
(define test
  '(cond ((atom ls) ls)
	 (t (let* ((lsd (dedup (cdr ls)))
		   (match (get-match (cadar ls) lsd)))
	      (cond (match lsd)
		    (t (cons (car ls) lsd)))))))
test
(define test (dedup (ssa (expand test))))
test
(define test (do-subs test))
test
(formatted (reverse test))
