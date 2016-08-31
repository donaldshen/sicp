(define tolerance 0.00001)
(define (abs x) (if (< x 0) (- 0 x) x))

(define (fixed-point f first-guess)
	(define (close-enough? v1 v2)
		(< (abs (- v1 v2)) tolerance)
	)
	(define (try guess)
		(let ((next (f guess)))
			(display "guess:")
			(display next)
			(newline)
			(if (close-enough? guess next)
				next
				(try next)
			)
		)
	)
	(try first-guess)
)

(define (ex36 x)
	(/ (log 1000) (log x))
)

(define (average-damp f)
    (lambda (x)
        (average x 
                 (f x))))

(define (cont-frac n d k)
	(define (cf i)
        (if (= k i)
            (/ (N k) (D k))
            (/ (N i)
               (+ (D i) (cf (+ i 1))))))

    (cf 1)
)

(define (another-cont-frac n d k)
	(define (iter k p)
		(if (= k 0)
			p
			(iter (- k 1) (/ (n k) (+ (d k) p)))
		)
	)
	(iter k 0)
)

(define (n i) 1)
(define (d i)
	(if (= 2 (remainder i 3))
		(* 2 (/ (+ i 1) 3))
		1
	)
)

(define (tan-cf x k)
	(define (d k) (- (* k 2) 1))
	(define (n k) (- 0 (* x x)))
	(- 0 (/ (cont-frac n d k) x))
)



