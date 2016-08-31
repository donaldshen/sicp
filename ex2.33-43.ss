(define (filter predicate sequence)
  (cond	((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence) (filter predicate (cdr sequence)))
	 )
	(else (filter predicate (cdr sequence)))
	)
  )

(define nil '())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))
      )
  )

(define (map3 p sequence)
  (accumulate 
   (lambda (x y)
     (cons (p x) y)
     ) 
   nil sequence)
  )

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1)
  )

(define (length seq)
  (accumulate
   (lambda (x y)
     (+ 1 y)
     )
   0 seq)
  )

(define (horner-eval x coefficient-seq)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+	this-coeff
	(* x higher-terms)
	)
     )
   0 coefficient-seq)
  )

(define (count-leaves t)
  (accumulate
   (lambda (x y)
     (if (pair? x)
	 (+ (count-leaves x) y)
	 (+ 1 y)
	 )
     )
   0 t)
  )

(define (count-leaves2 t)
  (accumulate + 0
	      (map (lambda (x) (if (pair? x) (count-leaves2 x) 1)) t)
	      )
  )

(define (accumulate-n op init seqs)
  (define (first seqs)
    (map car seqs)
    )
  (define (rest seqs)
    (map cdr seqs)
    )
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (first seqs)) (accumulate-n op init (rest seqs)))
      )
  )

(define (dot-product v w)
  (accumulate + 0 (map * v w))
  )

(define (matrix-*-vector m v)
  (define (f v2)
    (dot-product v v2)
    )
  (map f m)
  )

(define v1 (list 1 2 3))
(define m1 (list (list 1 2 3) (list 4 5 6)))

(define (transpose mat)
  (accumulate-n cons nil mat)
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
	   (matrix-*-vector cols v))
	 m)
    )
  )

(define m2 (list (list 7 8) (list 9 10) (list 11 12)))

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest)) (cdr rest))
	)
    )
  (iter initial seq)
  )

(define fold-right accumulate)

(define (reverse2 seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq)
  )

(define (reverse3 seq)
  (fold-left (lambda (x y) (cons y x)) nil seq)
  )
					;就是proc完每一项之后将其展开，让他们在同一个list下（flat：平坦）
(define (flatmap proc seqs)
  (accumulate append nil (map proc seqs))
  )

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair)))
  )

(define (prime? n)
  (define (iter test)
    (if (> test (sqrt n))
	#t
	(if (= 0 (remainder n test))
	    #f
	    (iter (+ test 1))
	    )
	)
    )
  (if (< n 2)
      #f
      (iter 2)
      )
  )

(define (make-pair-sum pair)
  (append pair (list (+ (car pair) (cadr pair))))
  )
					;先生成1到n的序列l1。对l1里的每一项i，先替换成1到（i－1）的序列li，再将li里的每一项j替换成i和j的序列。此时l1里的每一项已变成对于每一个i与i与1之间的所有序列的序列new li。最后把所有new li的元素合并到一个序列中。
(define (generate n)
  (accumulate
   append
   nil
   (map 	(lambda (i)
		  (map 	(lambda (j) (list i j)) 
			(enumerate-interval 1 (- i 1))
			)
		  )
		(enumerate-interval 1 n)
		)
   )
  )

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map 	(lambda (j) (list i j)) 
		(enumerate-interval 1 (- i 1))
		)
     )
   (enumerate-interval 1 n)
   )
  )

(define (enumerate-interval beg end)
  (if (> beg end)
      nil
      (cons beg (enumerate-interval (+ beg 1) end))
      )
  )

(define (prime-sum-pairs n)
  (map	make-pair-sum
	(filter prime-sum?
		(unique-pairs n)
		)
	)
  )

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap
       (lambda (x)
	 (map	(lambda (p)
		  (cons x p)
		  )
		(permutations (remove x s))
		)
	 )
       s
       )
      )
  )

(define (remove x s)
  (filter (lambda (y) (not (= x y))) s)
  )

(define (unique-triples n)
  (flatmap
   (lambda (x)
     (map	(lambda (y)
		  (cons x y)
		  )
		(unique-pairs (- x 1))
		)
     )
   (enumerate-interval 1 n)
   )
  )

(define (triple-sum-equal-to? sum triple)
  (= sum
     (+ (car triple)
	(cadr triple)
	(caddr triple))))

;;ex2.42左下角是(0 . 0)
(define (queens board-size)
  (let queen-cols ((k 1))
    (if (> k board-size)
	(list empty-board)
	(filter safe?
		(flatmap (lambda (rest-of-queens)
			   (map (lambda (new-row)
				  (adjoin-position new-row k rest-of-queens))
				(enumerate-interval 1 board-size)))
			 (queen-cols (+ k 1)))))))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k)
	rest-of-queens))
(define empty-board '())
(define (safe? positions)
  (define (not-safe? p1 p2)
    (let ((row1 (car p1))
	  (col1 (cdr p1))
	  (row2 (car p2))
	  (col2 (cdr p2)))
    (or (eq? row1 row2)
	(eq? (- row1 col1)
	     (- row2 col2))
	(eq? (+ row1 col1)
	     (+ row2 col2)))))
  (if (null? positions)
      true
      (let ((newp (car positions)))
	(let loop ((restp (cdr positions)))
	  (cond ((null? restp)
		 true)
		((not-safe? newp (car restp))
		 false)
		(else (loop (cdr restp))))))))
(for-each (lambda (pos)
	    (newline)
	    (display pos))
	  (queens 8))
