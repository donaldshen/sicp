					;ch5.5.1编译器的结构（final chapter！！）
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
	 (compile-self-evaluating exp target linkage))
	((variable? exp)
	 (compile-variable exp target linkage))
	((quoted? exp)
	 (compile-quoted exp target linkage))
	((assignment? exp)
	 (compile-assignment exp target linkage))
	((definition? exp)
	 (compile-definition exp target linkage))
	((if? exp)
	 (compile-if exp target linkage))
	((lambda? exp)
	 (compile-lambda exp target linkage))
	((begin? exp)
	 (compile-sequence (begin-actions exp) target linkage))
	((cond? exp)
	 (compile (cond->if exp) target linkage))
					;ex5.38
	((+? exp)
	 (compile-+ exp target linkage))
					;ex5.43
	((let? exp)
	 (compile (let->combination exp) target linkage))
	((application? exp)
	 (compile-application exp target linkage))
	(else (error "Unknown expression type -- COMPILE" exp))))
;;needs，是该指令序列从外部需要得到值的reg的集合；
;;modifies，是该指令序列会像其赋值的reg的集合
(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))
(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

					;ch5.5.2表达式的编译
;;初次使用反引号`backquote
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
	 (make-instruction-sequence '(continue)
				    '()
				    '((goto (reg continue)))))
	((eq? linkage 'next)
	 (empty-instruction-sequence))
	(else (make-instruction-sequence '()
					 '()
					 `((goto (label ,linkage)))))))
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
	      instruction-sequence
	      (compile-linkage linkage)))
					;简单表达式的编译
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence '()
					       (list target)
					       `((assign ,target
							 (const ,exp))))))
(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence
		     '()
		     (list target)
		     `((assign ,target
			       (const ,(text-of-quotation exp)))))))
(define (compile-variable exp target linkage)
  (end-with-linkage linkage
		    (make-instruction-sequence
		     '(env)
		     (list target)
		     `((assign ,target
			       (op lookup-variable-value)
			       (const ,exp)
			       (reg env))))))
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
	(get-value-code (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
		      (preserving '(env)
				  get-value-code
				  (make-instruction-sequence
				   '(val env)
				   (list target)
				   `((perform (op set-variable-value!)
					      (const ,var)
					      (reg val)
					      (reg env))
				     (assign ,target (const ok))))))))
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
	(get-value-code (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
		      (preserving '(env)
				  get-value-code
				  (make-instruction-sequence
				   '(env val)
				   (list target)
				   `((perform (op define-variable!)
					      (const ,var)
					      (reg val)
					      (reg env))
				     (assign ,target (const ok))))))))

					;条件表达式的编译
					;help function

(define new-label-number
  (let ((label-counter 0))
    (lambda ()
      (set! label-counter (+ 1 label-counter))
      label-counter)))
(define (make-label name)
  (string->symbol (string-append (symbol->string name)
				 (number->string (new-label-number)))))
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
	(f-branch (make-label 'false-branch))
	(after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next)
				  after-if
				  linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
	    (c-code (compile (if-consequent exp) target consequent-linkage))
	    (a-code (compile (if-alternative exp) target linkage)))
	(preserving '(env continue)
		    p-code
		    (append-instruction-sequences
		     (make-instruction-sequence
		      '(val)
		      '()
		      `((test (op false?) (reg val))
			(branch (label ,f-branch))))
		     (parallel-instruction-sequences
		      (append-instruction-sequences t-branch
						    c-code)
		      (append-instruction-sequences f-branch
						    a-code))
		     after-if))))))

					;表达式序列的编译
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
		  (compile (first-exp seq) target 'next)
		  (compile-sequence (rest-exps seq) target linkage))))

					;lambda表达式
					;help function
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc)
  (list-ref c-proc 1))
(define (compiled-procedure-env c-proc)
  (list-ref c-proc 2))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
	(after-lambda (make-label 'after-lambda)))
    ;;如果是next，跳到过程体下方
    (let ((lambda-linkage (if (eq? linkage 'next)
			      after-lambda
			      linkage)))
      (append-instruction-sequences
       ;;body的部分在end-with-linkage后面，所以只能通过proc-entry来访问
       (tack-on-instruction-sequence
	(end-with-linkage lambda-linkage
			  (make-instruction-sequence
			   '(env)
			   (list target)
			   `((assign ,target
				     (op make-compiled-procedure)
				     (label ,proc-entry)
				     (reg env)))))
	(compile-lambda-body exp proc-entry))
       after-lambda))))
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
	(assign env (op compiled-procedure-env) (reg proc))
	(assign env
		(op extend-environment) (const ,formals) (reg argl) (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

					;组合式的编译
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
	(operand-codes (map (lambda (operand)
			      (compile operand 'val 'next))
			    (operands exp))))
    (preserving '(env continue)
		proc-code
		(preserving '(proc continue)
			    (construct-arglist operand-codes)
			    (compile-procedure-call target linkage)))))
(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
	(make-instruction-sequence '()
				   '(argl)
				   '((assign argl (const ()))))
	(let ((code-to-get-last-arg
	       (append-instruction-sequences
		(car operand-codes)
		(make-instruction-sequence '(val)
					   '(argl)
					   '((assign argl (op list) (reg val)))))))
	  (if (null? (cdr operand-codes))
	      code-to-get-last-arg
	      (preserving '(env)
			  code-to-get-last-arg
			  (code-to-get-rest-args (cdr operand-codes))))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
	 (preserving '(argl)
		     (car operand-codes)
		     (make-instruction-sequence
		      '(val argl)
		      '(argl)
		      '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
	code-for-next-arg
	(preserving '(env)
		    code-for-next-arg
		    (code-to-get-rest-args (cdr operand-codes))))))

					;过程应用
;;就算是简单应用，如(> n 1)，也会modified all-regs
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
	(compiled-branch (make-label 'compiled-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next)
				after-call
				linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc)
				  '()
				  `((test (op primitive-procedure?) (reg proc))
				    (branch (label ,primitive-branch))))
       ;;因为compile-pro-appl会modified all-regs
       (parallel-instruction-sequences
	(append-instruction-sequences compiled-branch
				      (compile-proc-appl target
							 compiled-linkage))
	(append-instruction-sequences primitive-branch
				      (end-with-linkage
				       linkage
				       (make-instruction-sequence
					'(proc argl)
					(list target)
					`((assign ,target
						  (op apply-primitive-procedure)
						  (reg proc)
						  (reg argl)))))))
       after-call))))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
	      (not (eq? linkage 'return)))
	 (make-instruction-sequence '(proc)
				    all-regs
				    `((assign continue (label ,linkage))
				      (assign val
					      (op compiled-procedure-entry)
					      (reg proc))
				      (goto (reg val)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence '(proc)
				      all-regs
				      `((assign continue (label ,proc-return))
					(assign val
						(op compiled-procedure-entry)
						(reg proc))
					(goto (reg val))
					,proc-return
					(assign ,target (reg val))
					(goto (label ,linkage))))))
	((and (eq? target 'val)
	      (eq? linkage 'return))
	 (make-instruction-sequence '(proc continue)
				    all-regs
				    '((assign val
					      (op compiled-procedure-entry)
					      (reg proc))
				      (goto (reg val)))))
	((and (not (eq? target 'val))
	      (eq? linkage 'return))
	 (error "return linkage, target not val -- COMPILE" target))))
(define all-regs '(env proc val argl continue))

					;ch5.5.4指令序列的组合
(define (registers-needed s)
  (if (symbol? s)
      '()
      (car s)))
(define (registers-modified s)
  (if (symbol? s)
      '()
      (list-ref s 1)))
(define (statements s)
  (if (symbol? s)
      (list s)
      (list-ref s 2)))
(define (needs-register? seq reg)
  (memq reg
	(registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg
	(registers-modified seq)))
;;简单合并，调整needs和modifies
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
		 ;;seq2的needed里除去seq1改动过的reg，其值在seq1中得到，不需来自外部
		 (list-difference (registers-needed seq2)
				  (registers-modified seq1)))
     (list-union (registers-modified seq1)
		 (registers-modified seq2))
     (append (statements seq1)
	     (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
	(empty-instruction-sequence)
	(append-2-sequences (car seqs)
			    (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))
(define (list-union s1 s2)
  (cond ((null? s1) s2)
	((memq (car s1) s2)
	 (list-union (cdr s1)
		     s2))
	(else (cons (car s1)
		    (list-union (cdr s1)
				s2)))))
;;从s1中减去s2
(define (list-difference s1 s2)
  (cond ((null? s1) '())
	((memq (car s1) s2)
	 (list-difference (cdr s1)
			  s2))
	(else (cons (car s1)
		    (list-difference (cdr s1)
				     s2)))))
;;合并后对于regs里的reg，两段指令得到相同的值
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
	;;若该reg会被seq1修改同时被seq2需要
	(if (and (needs-register? seq2
				  first-reg)
		 (modifies-register? seq1
				     first-reg))
	    (preserving (cdr regs)
			(make-instruction-sequence
			 ;;将该reg并入seq1的需要
			 (list-union (list first-reg)
				     (registers-needed seq1))
			 ;;从seq1的修改里除去该reg
			 (list-difference (registers-modified seq1)
					  (list first-reg))
			 (append `((save ,first-reg))
				 (statements seq1)
				 `((restore ,first-reg))))
			seq2)
	    (preserving (cdr regs) seq1 seq2)))))
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (registers-needed seq)
			     (registers-modified seq)
			     (append (statements seq)
				     (statements body-seq))))
;;与append-instruction-sequences的区别是，seq1,seq2绝不会顺序执行
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence (list-union (registers-needed seq1)
					 (registers-needed seq2))
			     (list-union (registers-modified seq1)
					 (registers-modified seq2))
			     (append (statements seq1) (statements seq2))))

					;ch5.5.5编译代码的实例
;; (define test (compile '(define (add1 x)
;; 			 (+ x 1))
;; 		      'val
;; 		      'next))

;; (define test2 (compile '(define a 1)
;; 		       'val
;; 		       'next))
(define (compile-test exp)
  (compile exp 'val 'next))
;; (define t3 (compile-test '(define (factorial n)
;; 			    (if (= n 1)
;; 				1
;; 				(* n (factorial (- n 1)))))))
(define (display-s i)
  (display-l (statements i)))

;; (define t4 (compile-test '(define (factorial n)
;; 			    (define (iter product counter)
;; 			      (if (> counter n)
;; 				  product
;; 				  (iter (* product counter)
;; 					(+ counter 1))))
;; 			    (iter 1 1))))

					;ex5.33
					;t3
;; ;;构建过程，并跳过具体过程代码
;; (assign val (op make-compiled-procedure) (label entry7) (reg env))
;; (goto (label after-lambda6))

;; ;;调用factorial时，由此开始
;; entry7
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; ;;开始过程体
;; (save continue)
;; (save env)
;; ;;计算(= n 1)
;; (assign proc (op lookup-variable-value) (const =) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch22))

;; compiled-branch21
;; (assign continue (label after-call20))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch22
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;; ;;现在val里保存了(= n 1)的值
;; after-call20
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch9))

;; true-branch10
;; (assign val (const 1))
;; (goto (reg continue))

;; ;;计算(* n (factorial (- n 1)))
;; false-branch9
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; ;;每次递归前，都要将以下寄存器入栈
;; (save continue)
;; (save proc)
;; (save env)
;; ;;计算(factorial (- n 1))
;; (assign proc (op lookup-variable-value) (const factorial) (reg env))
;; (save proc)
;; ;;计算(- n 1)
;; (assign proc (op lookup-variable-value) (const -) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch13))

;; compiled-branch12
;; (assign continue (label after-call11))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch13
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;; after-call11
;; ;;val保存了(- n 1)的结果
;; (assign argl (op list) (reg val))
;; ;;proc是factorial
;; (restore proc)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch16))

;; compiled-branch15
;; (assign continue (label after-call14))
;; ;;递归开始
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch16
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;; after-call14
;; ;;将(factorial (- n 1))的结果加入argl中
;; (assign argl (op list) (reg val))
;; ;;计算n，并加入到argl中
;; (restore env)
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; 计算(* n (factorial (- n 1)))
;; (restore proc)
;; (restore continue)
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch19))

;; compiled-branch18
;; ;;尾递归，此时linkage是return
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch19
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))

;; after-call17

;; after-if8

;; after-lambda6
;; (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; (assign val (const ok))

					;ex5.34
					;t4
;; (assign val (op make-compiled-procedure) (label entry24) (reg env))
;; (goto (label after-lambda23))

;; entry24
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; ;;生成(define (iter product counter) ...)
;; (assign val (op make-compiled-procedure) (label entry29) (reg env))
;; (goto (label after-lambda28))

;; entry29
;; ;;设置环境啦
;; (assign env (op compiled-procedure-env) (reg proc))
;; (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;; ;;计算(> counter n)
;; (save continue)
;; (save env)
;; (assign proc (op lookup-variable-value) (const >) (reg env))
;; (assign val (op lookup-variable-value) (const n) (reg env))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch44))

;; compiled-branch43
;; (assign continue (label after-call42))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch44
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;; after-call42
;; ;;虽然(> counter n)并未改变env。但过程应用的modified是all-regs，下面紧接着是另一个过程应用，need env，所以会产生这个步骤
;; (restore env)
;; (restore continue)
;; (test (op false?) (reg val))
;; (branch (label false-branch31))

;; true-branch32
;; (assign val (op lookup-variable-value) (const product) (reg env))
;; (goto (reg continue))

;; false-branch31
;; (assign proc (op lookup-variable-value) (const iter) (reg env))
;; ;;计算(+ counter 1)，不会马上给回counter，不用担心product
;; (save continue)
;; (save proc)
;; (save env)
;; (assign proc (op lookup-variable-value) (const +) (reg env))
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch38))

;; compiled-branch37
;; (assign continue (label after-call36))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch38
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;; after-call36
;; (assign argl (op list) (reg val))
;; ;;计算(* product counter)
;; (restore env)
;; (save argl)
;; (assign proc (op lookup-variable-value) (const *) (reg env))
;; (assign val (op lookup-variable-value) (const counter) (reg env))
;; (assign argl (op list) (reg val))
;; (assign val (op lookup-variable-value) (const product) (reg env))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch35))

;; compiled-branch34
;; (assign continue (label after-call33))
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch35
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

;; after-call33
;; (restore argl)
;; ;;给iter新的argl构建完毕
;; (assign argl (op cons) (reg val) (reg argl))
;; (restore proc)
;; (restore continue)
;; ;;至此，栈又恢复到上次调用iter的情况
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch41))

;; compiled-branch40
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch41
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))

;; ;;用不上了
;; after-call39
;; after-if30

;; ;;得到(define (iter ..) ...)
;; after-lambda28
;; (perform (op define-variable!) (const iter) (reg val) (reg env))
;; (assign val (const ok))
;; ;;计算(iter 1 1)
;; (assign proc (op lookup-variable-value) (const iter) (reg env))
;; ;;构建argl
;; (assign val (const 1))
;; (assign argl (op list) (reg val))
;; (assign val (const 1))
;; (assign argl (op cons) (reg val) (reg argl))
;; (test (op primitive-procedure?) (reg proc))
;; (branch (label primitive-branch27))

;; ;;最后一条表达式，所以不用回到after-call25
;; compiled-branch26
;; (assign val (op compiled-procedure-entry) (reg proc))
;; (goto (reg val))

;; primitive-branch27
;; (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;; (goto (reg continue))

;; after-call25

;; after-lambda23
;; (perform (op define-variable!) (const factorial) (reg val) (reg env))
;; (assign val (const ok))

					;ex5.35
(define (f x)
  (+ x
     (g (+ x 2))))

					;ex5.36
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '()
				 '(argl)
				 '((assign argl (const ()))))
      (let ((code-to-get-last-arg
	     (append-instruction-sequences
	      (car operand-codes)
	      (make-instruction-sequence '(val)
					 '(argl)
					 '((assign argl (op list) (reg val)))))))
	(if (null? (cdr operand-codes))
	    code-to-get-last-arg
	    (preserving '(env)
			code-to-get-last-arg
			(code-to-get-rest-args (cdr operand-codes)))))))
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
	 (preserving '(argl)
		     (car operand-codes)
		     (make-instruction-sequence
		      '(val argl)
		      '(argl)
		      '((assign argl (op append) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
	code-for-next-arg
	(preserving '(env)
		    code-for-next-arg
		    (code-to-get-rest-args (cdr operand-codes))))))
;; (define t5 (compile-test '(define (factorial n)
;; 			    (if (= n 1)
;; 				1
;; 				(* (factorial (- n 1)) n)))))

					;ex5.38
(define (+? exp)
  (tagged-list? exp '+))
;;设定只处理两个参数的情况
(define (spread-arguments argl)
  (let ((operand-code1 (compile (car argl) 'arg1 'next))
	(operand-code2 (compile (cadr argl) 'arg2 'next)))
    (preserving '(env)
		operand-code1
		(make-instruction-sequence
		 (list-union '(arg1)
			     (registers-needed operand-code2))
		 (list-difference (registers-modified operand-code2)
				  '(arg1))
		 (append '((save arg1))
			 (statements operand-code2)
			 '((restore arg1)))))))
(define (compile-+ exp target linkage)
  (let ((operand-codes (spread-arguments (operands exp))))
    (end-with-linkage
     linkage
     (preserving '(continue)
		 operand-codes
		 (make-instruction-sequence
		  '()
		  `(target)
		  `((assign ,target (op +) (reg arg1) (reg arg2))))))))

(define t6 (compile-test '(+ a (+ 3 1))))
(define t7 (compile-test '(+ (+ a 1) (+ b 2))))
(define (display-t exp)
  (display-s (compile-test exp)))

(define (compile-++ exp target linkage)
  (compile-+ (construct exp) target linkage))
(define (construct exp)
  (if (> (length (operands exp))
	 2)
      (append (list (car exp)
		    (cadr exp))
	      (list (append (list (car exp))
			    (cddr exp))))
      exp))
      
					;ch5.5.6
					;ex5.39
(define (lexical-address-lookup address env)
  (let ((value (cadr (list-ref (list-ref env
					 (car address))
			       (cadr address)))))
    (if (eq? value
	     '*unassigned*)
	(error "Unassigned variable! -- LEXICAL-ADDRESS-LOOKUP" address)
	value)))
(define (lexical-address-set! address env value)
  (let ((binding (list-ref (list-ref env
				     (car address))
			   (cadr address))))
    (set-cdr! binding value)))

					;ex5.40
;; (define (compile-lambda-body exp proc-entry ct-env)
;;   (let ((formals (lambda-parameters exp)))
;;     (append-instruction-sequences
;;      (make-instruction-sequence
;;       '(env proc argl)
;;       '(env)
;;       `(,proc-entry
;; 	(assign env (op compiled-procedure-env) (reg proc))
;; 	(assign env
;; 		(op extend-environment) (const ,formals) (reg argl) (reg env))))
;;      (compile-sequence (lambda-body exp)
;; 		       'val
;; 		       'return
;; 		       (extend-ct-env formals
;; 				      ct-env)))))
(define (extend-ct-env variables ct-env)
  (cons ct-env))
(define empty-ct-env '())
					;ex5.41
(define (find-variable var ct-env)
  (let ((f-ref 0))
    (define (find var frame)
      (if (eq? var (car frame))
	  0
	  (+ 1 (find var (cdr frame)))))
    (define (iter ct-env)
      (cond ((null? ct-env)
	     'not-found)
	    ((memq? var (car ct-env))
	     (list f-ref (find var (car ct-env))))
	    (else (set! f-ref (+ 1 f-ref))
		  (iter (cdr ct-env)))))
    (iter ct-env)))

					;ex5.42
;; (define (compile-variable exp target linkage ct-env)
;;   (let ((address (find-variable exp ct-env)))
;;     (end-with-linkage linkage
;; 		      (make-instruction-sequence
;; 		       '(env)
;; 		       (list target)
;; 		       (if (eq? address 'not-found)
;; 			   `((assign ,target
;; 				     (op lookup-variable-value)
;; 				     (const ,exp)
;; 				     (reg env)))
;; 			   `((assign ,target
;; 				     (op lexical-address-lookup)
;; 				     (const ,address)
;; 				     (reg env))))))))
;; (define (compile-assignment exp target linkage ct-env)
;;   (let ((var (assignment-variable exp))
;; 	(get-value-code (compile (assignment-value exp) 'val 'next)))
;;     (let ((address (find-variable var ct-env))
;; 	  (end-with-linkage
;; 	   linkage
;; 	   (preserving '(env)
;; 		       get-value-code
;; 		       (make-instruction-sequence
;; 			'(val env)
;; 			(list target)
;; 			(if (eq? address 'not-found)
;; 			    `((perform (op lexical-address-set!)
;; 				       (const ,address)
;; 				       (reg env)
;; 				       (reg val)
;; 				       (assign ,target (const ok))))
;; 			    `((assign env (op get-global-environment))
;; 			      (perform (op set-variable-value!)
;; 				       (const ,var)
;; 				       (reg val)
;; 				       (reg env))
;; 			      (assign ,target (const ok)))))))))))

(define t8 (compile-test '((lambda (x y)
			     (lambda (a b c d e)
			       ((lambda (y z)
				  'hi)
				'fuck
				(+ c d x))))
			   3 4)))
(define t9 (compile-test '(let ((a 1)
				(b 2))
			    (+ a b))))
(define t10 (compile-test '(define (add1 x)
			     (define a 1)
			     (+ x a))))
;; (define (add1 x)
;;   ((lambda (a)
;;      (set! a 1)
;;      (+ x a))
;;    (quote '*unassigned*)))

					;ex5.44
(define (not-in-ct-env? operator ct-env)
  (eq? 'not-found
       (find-variable operator
		      ct-env)))
;; (define (+? exp ct-env)
;;   (and (not-in-ct-env? (operator exp) ct-env)
;;        (tagged-list? exp '+)))

					;ch5.5.7
					;编译代码与求值器的互连
(define (compile-and-go expression)
  (let ((instructions (assemble (statements (compile expression 'val 'return))
				eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
