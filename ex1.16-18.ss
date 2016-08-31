(define (expt b n)
	(expt-iter b n 1)
)

(define (expt-iter b n a)
	(cond
		((= n 0) a)
		((even? n)  (expt-iter (square b) (/ n 2) a))
		(else (expt-iter b (- n 1) (* b a)))
	)
)

(define (square x) (* x x))
(define (even? n) (= (remainder n 2) 0))

(define (multiply a b)
	(multiply-iter a b 0)
)

(define (multiply-iter a b sum)
	(cond
		((= b 0) sum)
		((even? b) (multiply-iter (* 2 a) (/ b 2) sum))
		(else (multiply-iter a (- b 1) (+ sum a)))
	)
)