(define (last-pair items)
	(if (null? (cdr items))
		(car items)
		(last-pair (cdr items))
	)
)

(define (reverse items)
	(if (null? items)
		'()
		(cons (reverse (cdr items)) (car items))
	)
)

(define (reverse2 lst)
    (iter lst '()))
;第一步就和表结构cons了，所以出来就是个表。而且cons的位置也值得学习
(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))

(define (same-parity x . y)
	(let ((z (remainder x 2)))
		(define (iter l result)
			(if (null? l)
				result
				(if (= z (remainder (car l) 2))
					(iter (cdr l) (cons (car l) result))
					(iter (cdr l) result)
				)
			)
		)
		(cons x (iter (reverse2 y) '()))
	)
)

(define (same-parity2 x . y)
	(let ((z (remainder x 2)))
		(define (iter l)
			(if (null? l)
				'()
				(if (= z (remainder (car l) 2))
					(cons (car l) (iter (cdr l)))
					(iter (cdr l))
				)
			)
		)
		(cons x (iter y))
	)
)

(define (map2 proc items)
	(if (null? items)
		'()
		(cons (proc (car items)) (map2 proc (cdr items)))
	)
)

(define (square-list items)
	(map2 (lambda (x) (* x x)) items)
)

(define (for-each proc items)
	(cond 
		((not (null? items)) 
			(proc (car items))
			(for-each proc (cdr items))
		)
	)
)













