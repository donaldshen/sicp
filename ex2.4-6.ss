(define (exp a b)
	(define (iter p b)
		(if (= b 0)
			p
			(iter (* a p) (- b 1))
		)
	)
	(iter 1 b)
)

(define (cons2 n d)
	(* (exp 2 n) (exp 3 d))
)

(define (car2 z)
	(define (iter p z)
		(if (= 1 (remainder z 2))
			p
			(iter (+ p 1) (/ z 2))
		)
	)
	(iter 0 z)
)

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))