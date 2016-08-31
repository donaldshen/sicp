					;ex5.21a
(define (count-leaves tree)
  (cond ((null? tree) 0)
	((not (pair? tree)) 1)
	(else (+ (count-leaves (car tree))
		 (count-leaves (cdr tree))))))
(controller
 (assign continue (label count-done))

 tree-loop
 (test (op null?) (reg tree))
 (branch (label null-tree))
 (test (op pair?) (reg tree))
 (branch (label pair-tree))
 (assign val (const 1))
 (goto (reg continue))

 null-tree
 (assign val (const 0))
 (goto (reg continue))

 pair-tree
 (save continue)
 (save tree)
 (assign tree (op car) (reg tree))
 (assign continue (label after-left-tree))
 (goto (label tree-loop))

 after-left-tree
 (restore tree)
 (assign tree (op cdr) (reg tree))
 (assign continue (label after-right-tree))
 (save val)
 (goto (label tree-loop))

 after-right-tree
 (restore left-val)
 (assign val (op +) (reg val) (reg left-val))
 (restore continue)
 (goto (reg continue))

 count-done)

					;b
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
	  ((not (pair? tree)) (+ n 1))
	  (else (count-iter (cdr tree)
			    (count-iter (car tree) n)))))
  (count-iter tree 0))

(define c-m
  (make-machine '()
		(list (list 'null? null?)
		      (list 'pair? pair?)
		      (list '+ +)
		      (list 'car car)
		      (list 'cdr cdr))
		'(controller
		  (assign n (const 0))
		  (assign continue (label iter-done))
		  
		  iter
		  (test (op null?) (reg tree))
		  (branch (label null-tree))
		  (test (op pair?) (reg tree))
		  (branch (label pair-tree))
		  (assign n (op +) (reg n) (const 1))
		  (goto (reg continue))

		  null-tree
		  (goto (reg continue))

		  pair-tree
		  (save continue)
		  (save tree)
		  (assign tree (op car) (reg tree))
		  (assign continue (label after-left-tree))
		  (goto (label iter))

		  after-left-tree
		  (restore tree)
		  (assign tree (op cdr) (reg tree))
		  (assign continue (label after-right-tree))
		  (goto (label iter))

		  after-right-tree
		  (restore continue)
		  (goto (reg continue))

		  iter-done)))

					;ex5.22
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (append x y)
  (if (null? x)
      y
      (cons (car x)
	    (append (cdr x) y))))
(define append-m
  (make-machine '()
		(list (list 'null? null?)
		      (list 'cdr cdr)
		      (list 'car car)
		      (list 'cons cons))
		'(controller
		  (assign continue (label append-done))

		  loop
		  (test (op null?) (reg x))
		  (branch (label null-x))
		  (save continue)
		  (assign continue (label cdr-done))
		  (assign car-x (op car) (reg x))
		  (save car-x)
		  (assign x (op cdr) (reg x))
		  (goto (label loop))

		  null-x
		  (assign x (reg y))
		  (goto (reg continue))

		  cdr-done
		  (restore car-x)
		  (assign x (op cons) (reg car-x) (reg x))
		  (restore continue)
		  (goto (reg continue))

		  append-done)))
(define (append! x y)
  (set-cdr! (last-pair x)
	    y)
  x)
(define (append! x y)
  (if (null? (cdr x))
      (set-cdr! x y)
      (append! (cdr x) y)))
(define append-m!
  (make-machine '()
		(list (list 'null? null?)
		      (list 'cdr cdr)
		      (list 'set-cdr! set-cdr!))
		'(controller
		  (assign iter-x (reg x))
		  
		  iter
		  (assign cdr-x (op cdr) (reg iter-x))
		  (test (op null?) (reg cdr-x))
		  (branch (label do-append))
		  (assign iter-x (op cdr) (reg iter-x))
		  (goto (label iter))

		  do-append
		  (perform (op set-cdr!) (reg iter-x) (reg y)))))
(set-register-contents! append-m! 'x '(1 2))
(set-register-contents! append-m! 'y '(34 2))
(start append-m!)
(get-register-contents append-m! 'x)

					;ch5.3.2
;;将整个流程模拟变动才知道其中的高深莫测！！！
(begin-garbage-collection
 ;;free、scan和root是指针
 (assign free (const 0))
 (assign scan (const 0))
 ;;root指向一个保存了所有有用数据指针的list，先转这个list，所以让old指向它
 (assign old (reg root))
 (assign relocate-continue (label reassign-root))
 (goto (label relocate-old-result-in-new))

 reassign-root
 (assign root (reg new))
 (goto (label gc-loop))

 gc-loop
 (test (op =) (reg scan) (reg free))
 (branch (label gc-flip))
 ;;scan为0时，old里被赋予的项是root表中的第一项，即是个指向the-cars表某处的指针
 (assign old (op vector-ref) (reg new-cars) (reg scan))
 (assign relocate-continue (label update-car))
 (goto (label relocate-old-result-in-new))

 update-car
 (perform (op vector-set!) (reg new-cars) (reg scan) (reg new))
 (assign old (op vector-ref) (reg new-cdrs) (reg scan))
 (assign relocate-continue (label update-cdr))
 (goto (label relocate-old-result-in-new))

 update-cdr
 (perform (op vector-set!) (reg new-cdrs) (reg scan) (reg new))
 (assign scan (op +) (reg scan) (const 1))
 (goto (label gc-loop))

 relocate-old-result-in-new
 ;;old里的东西将被转移
 (test (op pointer-to-pair?) (reg old))
 (branch (label pair))
 ;;
 (assign new (reg old))
 (goto (reg relocate-continue))

 pair
 ;;判断old数据是否已被转移
 (assign oldcr
	 (op vector-ref) (reg the-cars) (reg old))
 (test (op broken-heart?)
       (reg oldcr))
 (branch (label already-removed))
 ;;将当前free指针赋予new，free自增1
 (assign new
	 (reg free))
 (assign free
	 (op +) (reg free) (const 1))
 ;;转移开始
 (perform (op vector-set!)
	  (reg new-cars) (reg new) (reg oldcr))
 (assign oldcr
	 (op vector-ref) (reg the-cdrs) (reg old))
 (perform (op vector-set!)
	  (reg new-cdrs) (reg new) (reg oldcr))
 ;;标记old指针指向的数据broken-heart
 (perform (op vector-set!)
	  (reg the-cars) (reg old) (const broken-heart))
 (perform (op vector-set!)
	  (reg the-cdrs) (reg old) (reg new))
 (goto (reg relocate-continue))

 already-removed
 ;;可能root表里存在多个指针指向同一个数据，就像一人搬家时在原处留下新家位置，每个来访的人都会更新该地址
 (assign new (op vector-ref) (reg the-cdrs) (reg old))
 (goto (reg relocate-continue))

 gc-flip
 (assign temp (reg the-cdrs))
 (assign the-cdrs (reg new-cdrs))
 (assign new-cdrs (reg temp))
 (assign temp (reg the-cars))
 (assign the-cars (reg new-cars))
 (assign the-cdrs (reg temp)))
