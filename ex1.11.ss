(define (f n)
	(cond ((< n 3) n)
		(else 
			(+ 
				(f (- n 1)) 
				(* 2 (f (- n 2)))
				(* 3 (f (- n 3)))
			)
		)
	)
)

(define (another-f n)
	(f-iter 4 2 1 n)
)

(define (f-iter a b c count)
	(cond 
		((< count 3) count)
		((= count 3) a)
		((> count 3) 
			(f-iter 
				(+ a (* 2 b) (* 3 c))
				a
				b
				(- count 1)
			)
		)
	)
)