(defun (collatz n (acc 0))
  (if (> n 1)
    (collatz
      (if (> (% n 2) 0)
	(+ 1 (* 3 n))
	(/ n 2))
      (+ 1 acc))
    acc))

(defun (max-collatz n (best 0))
  (if (> n 1)
    (max-collatz (- n 1)
		 (let ((this (collatz n)))
		   (if (> this best) this best)))
    best))

(max-collatz 1000000)
