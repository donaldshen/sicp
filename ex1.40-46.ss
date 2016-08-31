(define (deriv g)
	(lambda (x) 
	(/ 
		(- 
			(g (+ x dx)) 
			(g x)
		)
		dx
	))
)

(define dx 0.00001)
(define (cube x) (* x x x))

(define (newton-transform g)
	(lambda (x)
		(-
			x
			(/
				(g x)
				((deriv g) x)
			)
		)
	)
)

(define (newtons-method g guess)
	(fixed-point (newton-transform g) guess)
)

(define (cubic a b c)
	(lambda (x)
		(+
			(cube x)
			(* a x x)
			(* b x)
			c
		)
	)
)

(define (double g)
	(lambda (x)
		(g (g x))
	)
)

(define (compose f g)
	(lambda (x)
		(f (g x))
	)
)

(define (repeated f n)
	(if (= n 1)
		;(lambda (x) x)
		f
		;(compose f (repeated f (- n 1))
		(lambda (x) (f ((repeated f (- n 1)) x)))
	)
)

(define (another-repeated f n)
	(define (iter g n)
		(if (= n 1)
			g
			(compose f (iter g (- n 1)))
		)
	)
	(iter f n)
)

(define (smooth f)
	(lambda (x)
		(/
			(+ (f (- x dx)) (f x) (f (+ x dx)))
			3
		)
	)
)

(define (smooth-n n)
	(repeated smooth n)
)

(define (iterative-improve g-e? ipv)
	(define (try guess)
		(let ((next (ipv guess)))
			(if (g-e? next)
				next
				(try next)
			)
		)
	)
	(lambda (first-guess) (try first-guess))
)

(define (new-sqrt x)
	(define (good-enough? guess)
		(< (abs (- x (* guess guess))) 0.00001)
	)
	(define (improve guess)
		(/ (+ guess (/ x guess)) 2)
	)
	((iterative-improve good-enough? improve) x)
)

(define (new-fixed-point f first-guess)
	(define (good-enough? guess)
		(< (abs (- guess (f guess))) 0.00001)
	)
	(f ((iterative-improve good-enough? f) first-guess))
)