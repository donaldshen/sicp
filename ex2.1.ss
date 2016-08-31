(define (abs x) (if (> x 0) x (- x)))

(define (make-rat n d)
	(define (make x y)
		(let ((g (abs (gcd x y))))
			(cons (/ x g) (/ y g))
		)
	)
	(cond 
		((= d 0) (error "d can't be 0!"))
		((= n 0) (cons n d))
		((> d 0) (make n d))
		(else (make (- n) (- d)))
	)
)
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (gcd a b)
	(if (= b 0)
		a
		(gcd b (remainder a b))
	)
)