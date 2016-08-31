					;机器模型
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
		((machine 'allocate-register) register-name))
	      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence) (assemble controller-text
						       machine))
    machine))

					;register
(define (make-register name)
  (let ((contents '*unassigned*)
	(trace-on false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
	    ((eq? message 'set)
	     (lambda (value)
	       (if trace-on
		   (begin (newline)
			  (display (list "reg:" name
					 " old-value:" contents
					 " new-value:" value))))
	       (set! contents value)))
	    ((eq? message 'trace-on) (set! trace-on true))
	    ((eq? message 'trace-off) (set! trace-on false))
	    (else (error "Unknown request -- REGISTER" message))))
    dispatch))
(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))
(define (trace-on-register machine register-name)
  ((get-register machine register-name) 'trace-on))
(define (trace-off-register machine register-name)
  ((get-register machine register-name) 'trace-off))

					;stack
(define (make-stack)
  (let ((s '())
	(number-pushes 0)
	(max-depth 0)
	(current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max max-depth current-depth)))
    (define (pop)
      (if (null? s)
	  (error "Empty stack -- POP")
	  (let ((top (car s)))
	    (set! s (cdr s))
	    (set! current-depth (- current-depth 1))
	    top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
		     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
	    ((eq? message 'pop) (pop))
	    ((eq? message 'initialize) (initialize))
	    ((eq? message 'print-statistics) (print-statistics))
	    (else (error "Unknown request -- STACK" message))))
    dispatch))
(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))
					;基本机器
(define (make-new-machine)
  (let ((pc (make-register 'pc))
	(flag (make-register 'flag))
	(stack (make-stack))
	(the-instruction-sequence '())
	(instruction-number 0)
	(trace-on false)
	(breakpoints '()))
    (let ((the-ops (list (list 'initialize-stack
			       (lambda ()
				 (stack 'initialize)))
			 (list 'print-stack-statistics
			       (lambda ()
				 (stack 'print-statistics)))))
	  (register-table (list (list 'pc pc)
				(list 'flag flag))))
      (define (allocate-register name)
      	(if (assoc name register-table)
      	    (error "Multiply defined register: " name)
      	    (set! register-table
      		  (cons (list name (make-register name))
      			register-table)))
      	'register-allocated)
      (define (lookup-register name)
	(let ((val (assoc name register-table)))
	  (if val
	      (list-ref val 1)
	      (begin (allocate-register name)
		     (lookup-register name)))))
      (define (execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (cond ((member? (instruction-text (car insts))
			      breakpoints)
		     (newline)
		     (display "BREAKPOINT!!"))
		    (else (inner-execute))))))
      (define (inner-execute)
	(let ((insts (get-contents pc)))
	  (if (null? insts)
	      'done
	      (let ((inst (car insts)))
		(cond (trace-on
		       (newline)
		       (display (list (instruction-label inst)
				      ": "
				      (instruction-text inst)))))
		((instruction-execution-proc inst))
		(set! instruction-number (+ instruction-number 1))
		(execute)))))
      (define (print-instruction-number)
	(display (list "current instruction number = " instruction-number))
	(set! instruction-number 0)
	(newline))
      (define (add-breakpoint label n)
	(set! breakpoints
	      (cons (instruction-text (list-ref (multi-assoc label
							     the-instruction-sequence)
						(- n 1)))
		    breakpoints)))
      
      (define (dispatch message)
	(cond ((eq? message 'start)
	       (set-contents! pc the-instruction-sequence)
	       (execute))
	      ((eq? message 'install-instruction-sequence)
	       (lambda (seq)
		 (set! the-instruction-sequence seq)))
	      ((eq? message 'allocate-register) allocate-register)
	      ((eq? message 'get-register) lookup-register)
	      ((eq? message 'install-operations)
	       (lambda (ops)
		 (set! the-ops (append the-ops ops))))
	      ((eq? message 'stack) stack)
	      ((eq? message 'operations) the-ops)
	      ((eq? message 'print-instruction-number) (print-instruction-number))
	      ((eq? message 'trace-on) (set! trace-on true))
	      ((eq? message 'trace-off) (set! trace-on false))
	      ((eq? message 'add-breakpoint) add-breakpoint)
	      ((eq? message 'proceed) (inner-execute))
	      ((eq? message 'cancel-a-breakpoint)
	       (lambda (label n)
		 (delete (instruction-text (list-ref (multi-assoc label
								  the-instruction-sequence)
						     (- n 1)))
			 breakpoints)))
	      ((eq? message 'cancel-all-breakpoints) (set! breakpoints '()))
	      (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

					;汇编程序
(define (assemble controller-text machine)
  (extract-labels controller-text
		  (lambda (insts labels)
		    (update-insts! insts (add-label labels) machine)
		    ;;((label text proc) ..)
		    insts)))
;;receive是个collector函数
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
		      (lambda (insts labels)
			(let ((next-inst (car text)))
			  (if (symbol? next-inst)
			      (if (assoc next-inst labels)
				  (error "Repeated label-name" next-inst)
				  (receive insts
					   (cons (make-label-entry next-inst
								   insts)
						 labels)))
			      (receive (cons (make-instruction next-inst)
					     insts)
				       labels)))))))
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
	(flag (get-register machine 'flag))
	(stack (machine 'stack))
	(ops (machine 'operations)))
    (for-each (lambda (inst)
		(set-instruction-execution-proc!
		 inst
		 (make-execution-procedure (instruction-text inst)
					   labels
					   machine
					   pc
					   flag
					   stack
					   ops)))
	      insts)))
(define (add-label labels)
  (for-each (lambda (l)
	      (let ((label (car l))
		    (insts (cdr l)))
		(for-each (lambda (inst)
			    (set-car! inst label))
			  insts)))
	    labels)
  labels)
;;(label text proc)
(define (make-instruction text)
  (list '() text '()))
(define (instruction-label inst)
  (list-ref inst 0))
(define (instruction-text inst)
  (list-ref inst 1))
(define (instruction-execution-proc inst)
  (list-ref inst 2))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cddr inst) proc))
(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
	(cdr val)
	(error "Undefined label -- ASSEMBLE" label-name))))

					;为指令生成执行过程
(define (make-execution-procedure inst labels machine pc flag stack ops)
  (let ((tag (car inst)))
    (cond ((eq? tag 'assign)
	   (make-assign inst machine labels ops pc))
	  ((eq? tag 'test)
	   (make-test inst machine labels ops flag pc))
	  ((eq? tag 'branch)
	   (make-branch inst machine labels flag pc))
	  ((eq? tag 'goto)
	   (make-goto inst machine labels pc))
	  ((eq? tag 'save)
	   (make-save inst machine stack pc))
	  ((eq? tag 'restore)
	   (make-restore inst machine stack pc))
	  ((eq? tag 'perform)
	   (make-perform inst machine labels ops pc))
	  ((eq? tag 'add1)
	   (make-add1 inst machine pc))
	  ((eq? tag 'switch)
	   (make-switch inst machine pc))
	  (else (error "Unknown instruction type -- ASSEMBLE" inst)))))

					;assign
(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine
			      (assign-reg-name inst)))
	(value-exp (assign-value-exp inst)))
    (let ((value-proc (if (operation-exp? value-exp)
			  (make-operation-exp value-exp
					      machine
					      labels
					      operations)
			  (make-primitive-exp (car value-exp)
					      machine
					      labels))))
      (lambda ()
	(set-contents! target (value-proc))
	(advance-pc pc)))))
(define (assign-reg-name assign-instruction)
  (list-ref assign-instruction 1))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))
(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

					;test
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
	(let ((condition-proc (make-operation-exp condition
						  machine
						  labels
						  operations)))
	  (lambda ()
	    (set-contents! flag (condition-proc))
	    (advance-pc pc)))
	(error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

					;branch
(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
	(let ((insts (lookup-label labels
				   (label-exp-label dest))))
	  (lambda ()
	    (if (get-contents flag)
		(set-contents! pc insts)
		(advance-pc pc))))
	(error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (list-ref branch-instruction 1))

					;goto
(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
	   (let ((insts (lookup-label labels
				      (label-exp-label dest))))
	     (lambda ()
	       (set-contents! pc insts))))
	  ((register-exp? dest)
	   (let ((reg (get-register machine
				    (register-exp-reg dest))))
	     (lambda ()
	       (set-contents! pc (get-contents reg)))))
	  (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (list-ref goto-instruction 1))

					;save
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

					;restore
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
			   (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (list-ref stack-instruction 1))

					;补充语法
					;perform
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
	(let ((action-proc (make-operation-exp action
					       machine
					       labels
					       operations)))
	  (lambda ()
	    (action-proc)
	    (advance-pc pc)))
	(error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst)
  (cdr inst))

					;add1
(define (make-add1 inst machine pc)
  (let ((reg (get-register machine
			   (add1-reg-name inst))))
    (lambda ()
      (set-contents! reg (+ (get-contents reg) 1))
      (advance-pc pc))))
(define (add1-reg-name inst)
  (list-ref inst 1))

					;switch
(define (make-switch inst machine pc)
  (let ((r1 (get-register machine
			  (first-reg-name inst)))
	(r2 (get-register machine
			  (second-reg-name inst))))
    (lambda ()
      (let ((temp (get-contents r1)))
	(set-contents! r1 (get-contents r2))
	(set-contents! r2 temp)
	(advance-pc pc)))))
(define (first-reg-name inst)
  (list-ref inst 1))
(define (second-reg-name inst)
  (list-ref inst 2))


					;primitive
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
	 (let ((c (constant-exp-value exp)))
	   (lambda ()
	     c)))
	((label-exp? exp)
	 (let ((insts (lookup-label labels
				    (label-exp-label exp))))
	   (lambda ()
	     insts)))
	((register-exp? exp)
	 (let ((r (get-register machine
				(register-exp-reg exp))))
	   (lambda ()
	     (get-contents r))))
	(else (error "Unknown expression type -- ASSEMBLE" exp))))
(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (list-ref exp 1))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (list-ref exp 1))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp)
  (list-ref exp 1))

					;operation
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
	(aprocs (map (lambda (e)
		       (make-primitive-exp e machine labels))
		     (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p))
		     aprocs)))))
(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
					;第一项的第二项：((op fuck) ..)
(define (operation-exp-op operation-exp)
  (list-ref (car operation-exp) 1))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
	(list-ref val 1)
	(error "Unknown operation -- ASSEMBLE" symbol))))

					;help-function
(define (multi-assoc i lst)
  (cond ((null? lst) '())
	((eq? i (caar lst))
	 (cons (car lst)
	       (multi-assoc i (cdr lst))))
	(else (multi-assoc i (cdr lst)))))
(define (delete item lst)
  (cond ((null? lst) '())
	((equal? item (car lst))
	 (delete item (cdr lst)))
	(else (cons (car lst)
		    (delete item (cdr lst))))))
(define (member? i lst)
  (cond ((null? lst) false)
	((eq? i (car lst)) lst)
	(else (member? i (cdr lst)))))

					;breakpoint
(define (set-breakpoint machine label n)
  ((machine 'add-breakpoint) label n))
(define (proceed-machine machine)
  (machine 'proceed))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-a-breakpoint) label n))
(define (cancel-all-breakpoints machine )
  (machine 'cancel-all-breakpoints))



					;real-machine
(define gcd-machine
  (make-machine '(a b t)
		(list (list 'rem remainder) (list '= =))
		'(test-b
		  (test (op =) (reg b) (const 0))
		  (branch (label gcd-done))
		  (assign t (op rem) (reg a) (reg b))
		  (assign a (reg b))
		  (assign b (reg t))
		  (goto (label test-b))
		  gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)

(define test-m
  (make-machine '(a)
		(list (list '= =)
		      (list '- -))
		'(start
		  (assign a (const b))
		  done)))
;;(get-register-contents test-m 'a)

(define expt-m
  (make-machine '(n counter product)
		(list (list '= =)
		      (list '- -)
		      (list '* *))
		'(controller
		  (assign counter (reg n))
		  (assign product (const 1))

		  expt-iter
		  (test (op =) (reg counter) (const 0))
		  (branch (label expt-done))
		  (assign counter (op -) (reg counter) (const 1))
		  (assign product (op *) (reg b) (reg product))
		  (goto (label expt-iter))

		  expt-done)))
(set-register-contents! expt-m 'b 2)
(set-register-contents! expt-m 'n 3)

(define expt-m2
  (make-machine '()
		(list (list '= =)
		      (list '- -)
		      (list '* *))
		'(controller
		  (assign continue (label expt-done))

		  expt-loop
		  (test (op =) (reg n) (const 0))
		  (branch (label base-case))
		  (save continue)
		  (assign n (op -) (reg n) (const 1))
		  (assign continue (label after-expt))
		  (goto (label expt-loop))

		  after-expt
		  (restore continue)
		  (assign val (op *) (reg b) (reg val))
		  (goto (reg continue))

		  base-case
		  (assign val (const 1))
		  (goto (reg continue))

		  expt-done)))
(set-register-contents! expt-m2 'b 3)
(set-register-contents! expt-m2 'n 2)
