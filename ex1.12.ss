(define (p x y)
	(if (or (= y 1) (= x y)) 
		1
		(+
			(p (- x 1) (- y 1))
			(p (- x 1) y) 
		)
	)
)