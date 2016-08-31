(list (list 1 2) (list 3 4))

(list (list 1 2) 3 4)

(cons (cons 1 (cons 2 '())) (cons 3 (cons 4 '())))

(list 1 (list 2 (list 3 4)))

(cons 
	1 
	(cons 
		(cons 
			2
			(cons 
				(cons 3 (cons 4 '()))
				'()
			)
		) 
		'()
	)
)

(define l1 (list 1 3 (list 5 7) 9))
(define l2 (list (list 7)))
(define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (cdr l1)))))
(car (car l2))
(cadr (cadr (cadr (cadr (cadr (cadr l3))))))


(define (is-list? sth)
	(if (pair? sth)
		(if (null? (cdr sth)) 
			#t
			(is-list? (cdr sth))
		)
		#f
	)
)

(define (deep-reverse L) 
	(if (pair? L) 
		(reverse2 (map deep-reverse L))
		L
	)
)

(define (append l1 l2)
	(if (null? l1)
		l2
		(cons (car l1) (append (cdr l1) l2))
	)
)

(define (fringe tree)
    	(cond
	 ((null? tree)
		tree
	 )
	 ((not (pair? tree))
		(list tree)
	 )
	 (else
		(append (fringe (car tree)) (fringe (cdr tree)))
	 )
	)
)    

(define (make-mobile left right)
	(list left right)
)

(define (make-branch length structure)
	(list length structure)
)

(define (left-branch mobile)
	(car mobile)
)

(define (right-branch mobile)
	(cadr mobile)
)

(define (branch-length branch)
	(car branch)
)

(define (branch-structure branch)
	(cadr branch)
)

(define (total-weight mobile)
	(+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile)))
)

(define (branch-weight branch)
	(let ((struct (branch-structure branch)))
		(if (pair? struct)
			(total-weight struct)
			struct
		)
	)
)

(define m1 (make-mobile (make-branch 1 2) (make-branch 3 (make-mobile (make-branch 4 5) (make-branch 6 7)))))

(define (balance? mobile)
	(let ((l-b (left-branch mobile)) (r-b (right-branch mobile)))
		(and
		 (if (pair? (branch-structure l-b))
			(balance? (branch-structure l-b))
			#t
		 )
		 (if (pair? (branch-structure r-b))
			(balance? (branch-structure r-b))
			#t
		 )
		 (= (movement l-b) (movement r-b))
		)
	)
)

(define (movement branch)
	(* (branch-length branch) (branch-weight branch))
)

(define m2 (make-mobile (make-branch 3 4) (make-branch 2 (make-mobile (make-branch 4 2) (make-branch 2 4)))))

(define (square x) (* x x))

(define (square-tree tree)
	(tree-map square tree)
)

(define (tree-map proc tree)
	(map 
	(lambda (sub-tree)
		(if (pair? sub-tree)
			(tree-map proc sub-tree)
			(proc sub-tree)
		)
	) tree)
)

(define (subsets s)
	(define (combine x)
		(cons (car s) x)
	)
	(if (null? s)
		(list '())
		(let ((rest (subsets (cdr s))))
			(append rest (map combine rest))
		)
	)
)




