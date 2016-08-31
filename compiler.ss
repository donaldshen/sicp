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
;;仅有编译lambda-body时linkage为return，保证调用完后要回到continue所在的位置
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
		    ;;predicate指令
		    p-code
		    (append-instruction-sequences
		     ;;select指令
		     (make-instruction-sequence
		      '(val)
		      '()
		      `((test (op false?) (reg val))
			(branch (label ,f-branch))))
		     (parallel-instruction-sequences
		      ;;consequence
		      (append-instruction-sequences t-branch
						    c-code)
		      ;;alternative
		      (append-instruction-sequences f-branch
						    a-code))
		     after-if))))))

					;表达式序列的编译
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      ;;linkage传给最后一条exp
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
			  ;;将编译好的compiled-procedure赋值给target
			  (make-instruction-sequence
			   '(env)
			   (list target)
			   `((assign ,target
				     (op make-compiled-procedure)
				     (label ,proc-entry)
				     ;;(const ,proc-entry)
				     (reg env)))))
	(compile-lambda-body exp proc-entry))
       after-lambda))))
(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     ;;先是extend env，默认此时argl里保存了formals的值
     (make-instruction-sequence
      '(env proc argl)
      '(env)
      `(,proc-entry
	(assign env (op compiled-procedure-env) (reg proc))
	(assign env
		(op extend-environment) (const ,formals) (reg argl) (reg env))))
     ;;唯一使用return的地方，意思是结尾使用(goto (reg continue))
     (compile-sequence (lambda-body exp) 'val 'return))))

					;组合式的编译
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
	(operand-codes (map (lambda (operand)
			      (compile operand 'val 'next))
			    (operands exp))))
    (preserving '(env continue)
		;;procedure
		proc-code
		(preserving '(proc continue)
			    ;;argl
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
		;;the last exp
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
		     ;;the last exp
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
	(compound-branch (make-label 'compound-branch))
	(after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next)
				after-call
				linkage)))
      (append-instruction-sequences
       ;;primitive or compiled?
       (make-instruction-sequence '(proc)
				  '()
				  `((test (op primitive-procedure?) (reg proc))
				    (branch (label ,primitive-branch))
				    (test (op compound-procedure?) (reg proc))
				    (branch (label ,compound-branch))))
       ;;因为compile-pro-appl会modified all-regs
       (parallel-instruction-sequences
	;;compiled-procedure
	(append-instruction-sequences compiled-branch
				      (compile-proc-appl target
							 compiled-linkage))
	(parallel-instruction-sequences
	 ;;compound-procedure
	 (append-instruction-sequences compound-branch
				       (compound-proc-appl target
							   compiled-linkage))
	 ;;primitive-procedure
	 (append-instruction-sequences primitive-branch
				       (end-with-linkage
					linkage
					(make-instruction-sequence
					 '(proc argl)
					 (list target)
					 `((assign ,target
						   (op apply-primitive-procedure)
						   (reg proc)
						   (reg argl))))))))
       after-call))))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val)
	      (not (eq? linkage 'return)))
	 ;;因为lambda过程最后一条是(goto (reg continue))
	 (make-instruction-sequence '(proc)
				    ;;此时无法知晓proc里会如何使用regs
				    all-regs
				    `((assign continue (label ,linkage))
				      (assign val
					      (op compiled-procedure-entry)
					      (reg proc))
				      (goto (reg val)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 ;;target只可能为proc
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
	 ;;此时，该过程应用必然是某lambda-body中的最后一条exp
	 (make-instruction-sequence '(proc continue)
				    all-regs
				    '((assign val
					      (op compiled-procedure-entry)
					      (reg proc))
				      (goto (reg val)))))
	((and (not (eq? target 'val))
	      (eq? linkage 'return))
	 (error "return linkage, target not val -- COMPILE" target))))
(define (compound-proc-appl target linkage)
  (cond ((and (eq? target 'val)
	      (not (eq? linkage 'return)))
	 (make-instruction-sequence '(proc)
				    all-regs
				    `((assign continue (label ,linkage))
				      (save continue)
				      (goto (reg compapp)))))
	((and (not (eq? target 'val))
	      (not (eq? linkage 'return)))
	 (let ((proc-return (make-label 'proc-return)))
	   (make-instruction-sequence '(proc)
				      all-regs
				      `((assign continue (label ,proc-return))
					(save continue)
					(goto (reg compapp))
					,proc-return
					(assign ,target (reg val))
					(goto (label ,linkage))))))
	((and (eq? target 'val)
	      (eq? linkage 'return))
	 ;;此时，该过程应用必然是某lambda-body中的最后一条exp
	 (make-instruction-sequence '(proc continue)
				    all-regs
				    '((save continue)
				      (goto (reg compapp)))))
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
;;普通合并，seqs顺序执行，适当调整needs和modifies
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
;;seq1,seq2顺序执行；但seq1,seq2得到相同的regs
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
;;只将body-seq的statements附在seq后头
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence (registers-needed seq)
			     (registers-modified seq)
			     (append (statements seq)
				     (statements body-seq))))
;;平行合并，只执行seq1,seq2中的一项
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence (list-union (registers-needed seq1)
					 (registers-needed seq2))
			     (list-union (registers-modified seq1)
					 (registers-modified seq2))
			     (append (statements seq1) (statements seq2))))



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
(define (help-+ exp target linkage)
  (let ((operand-codes (spread-arguments (operands exp))))
    (end-with-linkage
     linkage
     (preserving '(continue)
		 operand-codes
		 (make-instruction-sequence
		  '()
		  `(target)
		  `((assign ,target (op +) (reg arg1) (reg arg2))))))))


(define (compile-+ exp target linkage)
  (help-+ (construct exp) target linkage))
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

					;test function
(define (compile-test exp)
  (compile exp 'val 'next))
(define (display-s i)
  (display-l (statements i)))
(define (display-t exp)
  (display-s (compile-test exp)))
(define text '(define (factorial n)
		(if (= n 1)
		    1
		    (* (factorial (- n 1)) n))))
(define t2 '((lambda (x) (+ x 1)) 5))
;;这求值器到底该怎么做？每次输入代码都要和之前输入过的一起compile然后assemble再交由机器去执行吗？？
;;要在make-compiled-procedure里把assemble好的指令放进去？？
;;作孽啊！！make-primitive-exp就可以做到啊！我之前把compile-lambda里的(label ,proc-entry)改成(const ,proc-entry)就因为make-operation-exp里禁止用label做参数！坑爹的ex5.9！！！
(define t3 '(define (f x) (+ 1 (g x))))

					;ex5.48
(define (compile-and-run expression)
  (let ((instructions (compile-and-assemble expression)))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
(define (compile? exp)
  (tagged-list? exp 'compile))
;;(compile (quote exp))
(define (compile-exp exp)
  (list-ref (list-ref exp 1) 1))
(define (compile-and-assemble-in-eceval exp)
  (assemble (statements (compile exp 'val 'return))
	    eceval))

					;ex5.49
(define (create-ceval)
  (make-machine '()
	        (cons (list 'compile-and-assemble-in-ceval compile-and-assemble-in-ceval)
		      eceval-operations)			
		'(read-compile-eval-print-loop
		  (perform (op initialize-stack))
		  (perform (op prompt-for-input) (const ";;; C-EVAL input:"))
		  (assign exp (op read))
		  (assign env (op get-global-environment))
		  (assign val (op compile-and-assemble-in-ceval) (reg exp))
		  (assign continue (label print-result))
		  (goto (reg val))

		  print-result
		  (perform (op print-stack-statistics))
		  (perform (op announce-output) (const ";;; C-EVAL value:"))
		  (perform (op user-print) (reg val))
		  (goto (label read-compile-eval-print-loop)))))
(define (compile-and-assemble-in-ceval exp)
  (assemble (statements (compile exp 'val 'return))
	    ceval))
