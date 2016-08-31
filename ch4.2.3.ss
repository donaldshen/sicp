(define (cons x y)
  (lambda (m)
    (m x y)))
(define (car z)
  (z (lambda (x y) x)))
(define (cdr z)
  (z (lambda (x y)
       y)))
(define ones
  (cons 1 ones))
(define integers
  (cons 1 (map (lambda (x y)
		 (+ x y))
	       ones
	       integers)))

					;ex4.33
(define (display-infinite-list l)
  (define (displaying items index)
    (if (> index 10)
	(display "...")
	(begin (newline)
	       (display (car items))
	       (displaying (cdr items) (+ index 1)))))
  (displaying l 1))
