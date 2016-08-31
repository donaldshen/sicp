(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a) (sum term (next a) next b)
		)
	)
)

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (ex29 f a b n)
	(define h (/ (- b a) n))
	(define (add-h a) (+ a h))
	(define (add-double-h a) (+ a h h))
	(*
		(/ h 3)
		(+
			(+ (f a) (f (+ a (* n h))))
			(* 2 (sum f (+ a h) add-h (+ a (* (- n 1) h))))
			(* 2 (sum f (+ a h) add-double-h (+ a (* (- n 1) h))))
		)
	)
)

(define (simpson f a b n)
    
    (define h (/ (- b a) n))

    (define (y k)
        (f (+ a (* k h))))

    (define (factor k)
        (cond ((or (= k 0) (= k n))
                1)
              ((odd? k)
                4)
              (else
                2)))
    
    (define (term k)
        (* (factor k)
           (y k)))

    (define (next k)
        (+ k 1))

    (if (not (even? n))
        (error "n can't be odd")
        (* (/ h 3)
           (sum term (exact->inexact 0) next n))))

(define (another-sum term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))
		)
	)
	(iter a 0)
)

(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a) (accumulate combiner null-value term (next a) next b))
	)
)

(define (another-accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (combiner (term a) result))
		)
	)
	(iter a null-value)
)






