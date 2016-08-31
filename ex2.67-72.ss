(define (make-leaf symbol weight)
	(list 'leaf symbol weight)
)

(define (leaf? object)
	(eq? (car object) 'leaf)
)

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
	(list	left
		right
		(append (symbols left) (symbols right))
		(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
	(if (leaf? tree)
	    (list (symbol-leaf tree))
	    (caddr tree)))

(define (weight tree)
	(if (leaf? tree)
	    (weight-leaf tree)
	    (cadddr tree)))

(define (decode bits tree)
	(define (decode-1 bits current-branch)
		(if (null? bits)
		    '()
		    (let ((next-branch (choose-branch (car bits) current-branch)))
		    	 (if (leaf? next-branch)
			     (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
			     (decode-1 (cdr bits) next-branch)))))
	(decode-1 bits tree))

(define (choose-branch bit branch)
	(cond ((= bit 0) (left-branch branch))
	      ((= bit 1) (right-branch branch))
	      (else (error "bad bi -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
	(cond ((null? set) (list x))
	      ((< (weight x) (weight (car set))) (cons x set))
	      (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
	(if (null? pairs)
	    '()
	    (let ((pair (car pairs)))
	    	 (adjoin-set (make-leaf (car pair) (cadr pair))
		 	 (make-leaf-set (cdr pairs))))))

(define sample-tree
	(make-code-tree (make-leaf 'A 4)
	 (make-code-tree (make-leaf 'B 2)
	  (make-code-tree (make-leaf 'C 1)
	  		  (make-leaf 'D 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (encode message tree)
	(if (null? message)
	    '()
	    (append (encode-symbol (car message) tree)
	    	    (encode (cdr message) tree))))

(define (encode-symbol sym tree)
	(cond ((leaf? tree) '());可以优化重复判断的部分
	      ((not (in? sym (symbols tree))) (error "the symbol isn't in the tree"))
	      ((in? sym (symbols (left-branch tree))) (cons '0 (encode-symbol sym (left-branch tree))))
	      (else (cons '1 (encode-symbol sym (right-branch tree))))))


(define (in? sym syms)
	(if (null? syms)
	    #f
	    (or (eq? sym (car syms)) (in? sym (cdr syms)))))

(define message2 (decode sample-message sample-tree))

(define (generate-huffman-tree pairs)
	(successive-merge (make-leaf-set pairs)))

(define pairs1 '((a 4) (b 2) (c 1) (d 1)))

(define (successive-merge leafs)
	(if (null? (cdr leafs))
	    (car leafs)
	    (let ((new-code (make-code-tree (car leafs) (cadr leafs))))
		 (successive-merge (adjoin-set new-code (cddr leafs))))))

(define pairs2 '((a 2) (boom 1) (na 16) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))

(define tree2 (generate-huffman-tree pairs2))

