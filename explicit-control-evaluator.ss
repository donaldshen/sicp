(load "foundation-for-evaluator")
(load "register-machine")
(load "compiler")
					;help function
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))
(define (no-more-exps? seq)
  (null? seq))

(define the-global-environment (setup-environment))
(define (get-global-environment)
  the-global-environment)

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (init lst)
  (define (iter lst)
    (cond ((null? lst) '())
	  (else (cons `(list (quote ,(car lst)) ,(car lst))
		      (iter (cdr lst))))))
  (append '(list) (iter lst)))
(define (repl lst)
  (map (lambda (i)
	 (list-ref i 2))
       lst))
(define (display-l l)
  (for-each (lambda (i) (newline) (display i)) l))

(define procs-text
  '(prompt-for-input
    read
    get-global-environment
    announce-output
    user-print

    list
    cons
    car
    cdr
    +
    
    self-evaluating?
    
    variable?
    lookup-variable-value
    
    quoted?
    text-of-quotation

    assignment?
    assignment-variable
    assignment-value
    set-variable-value!

    definition?
    definition-variable
    definition-value
    define-variable!
    
    if?
    if-predicate
    true?
    false?
    if-alternative
    if-consequent

    lambda?
    lambda-parameters
    lambda-body
    make-procedure
    
    begin?
    begin-actions
    first-exp
    last-exp?
    rest-exps

    cond?
    cond->if
    
    cond-clauses
    no-more-exps?
    cond-else-clause?
    cond-predicate
    cond-actions
    
    let?
    let->combination
    
    application?
    operands
    operator
    empty-arglist
    no-operands?
    first-operand
    last-operand?
    adjoin-arg
    rest-operands
    primitive-procedure?
    apply-primitive-procedure

    compound-procedure?
    procedure-parameters
    procedure-environment
    extend-environment
    procedure-body

    compiled-procedure?
    make-compiled-procedure
    compiled-procedure-entry
    compiled-procedure-env
    compile?
    compile-exp
    compile-and-assemble-in-eceval
    ))
(define eceval-operations
  (list (list (quote prompt-for-input) prompt-for-input) (list (quote read) read) (list (quote get-global-environment) get-global-environment) (list (quote announce-output) announce-output) (list (quote user-print) user-print) (list (quote list) list) (list (quote cons) cons) (list (quote car) car) (list (quote cdr) cdr) (list (quote +) +) (list (quote self-evaluating?) self-evaluating?) (list (quote variable?) variable?) (list (quote lookup-variable-value) lookup-variable-value) (list (quote quoted?) quoted?) (list (quote text-of-quotation) text-of-quotation) (list (quote assignment?) assignment?) (list (quote assignment-variable) assignment-variable) (list (quote assignment-value) assignment-value) (list (quote set-variable-value!) set-variable-value!) (list (quote definition?) definition?) (list (quote definition-variable) definition-variable) (list (quote definition-value) definition-value) (list (quote define-variable!) define-variable!) (list (quote if?) if?) (list (quote if-predicate) if-predicate) (list (quote true?) true?) (list (quote false?) false?) (list (quote if-alternative) if-alternative) (list (quote if-consequent) if-consequent) (list (quote lambda?) lambda?) (list (quote lambda-parameters) lambda-parameters) (list (quote lambda-body) lambda-body) (list (quote make-procedure) make-procedure) (list (quote begin?) begin?) (list (quote begin-actions) begin-actions) (list (quote first-exp) first-exp) (list (quote last-exp?) last-exp?) (list (quote rest-exps) rest-exps) (list (quote cond?) cond?) (list (quote cond->if) cond->if) (list (quote cond-clauses) cond-clauses) (list (quote no-more-exps?) no-more-exps?) (list (quote cond-else-clause?) cond-else-clause?) (list (quote cond-predicate) cond-predicate) (list (quote cond-actions) cond-actions) (list (quote let?) let?) (list (quote let->combination) let->combination) (list (quote application?) application?) (list (quote operands) operands) (list (quote operator) operator) (list (quote empty-arglist) empty-arglist) (list (quote no-operands?) no-operands?) (list (quote first-operand) first-operand) (list (quote last-operand?) last-operand?) (list (quote adjoin-arg) adjoin-arg) (list (quote rest-operands) rest-operands) (list (quote primitive-procedure?) primitive-procedure?) (list (quote apply-primitive-procedure) apply-primitive-procedure) (list (quote compound-procedure?) compound-procedure?) (list (quote procedure-parameters) procedure-parameters) (list (quote procedure-environment) procedure-environment) (list (quote extend-environment) extend-environment) (list (quote procedure-body) procedure-body) (list (quote compiled-procedure?) compiled-procedure?) (list (quote make-compiled-procedure) make-compiled-procedure) (list (quote compiled-procedure-entry) compiled-procedure-entry) (list (quote compiled-procedure-env) compiled-procedure-env) (list (quote compile?) compile?) (list (quote compile-exp) compile-exp) (list (quote compile-and-assemble-in-eceval) compile-and-assemble-in-eceval)))

	  


					;ch5.4.1
(define eceval
  (make-machine '()
		eceval-operations
		'((assign compapp (label compound-apply))
		  (branch (label external-entry))

		  read-eval-print-loop
		  (perform (op initialize-stack))
		  (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
		  (assign exp (op read))
		  (assign env (op get-global-environment))
		  (assign continue (label print-result))
		  (goto (label eval-dispatch))

		  external-entry
		  (perform (op initialize-stack))
		  (assign env (op get-global-environment))
		  (assign continue (label print-result))
		  (goto (reg val))

		  print-result
		  (perform (op print-stack-statistics))
		  (perform (op announce-output) (const ";;; EC-Eval value:"))
		  (perform (op user-print) (reg val))
		  (goto (label read-eval-print-loop))

		  unknown-expression-type
		  (assign val (const unknown-expression-type-error))
		  (goto (label signal-error))

		  unknown-procedure-type
		  (restore continue)
		  (assign val (const unknown-procedure-type-error))
		  (goto (label signal-error))

		  signal-error
		  (perform (op user-print) (reg val))
		  (goto (label read-eval-print-loop))


		  eval-dispatch
		  (test (op self-evaluating?) (reg exp))
		  (branch (label ev-self-eval))
		  (test (op variable?) (reg exp))
		  (branch (label ev-variable))
		  (test (op quoted?) (reg exp))
		  (branch (label ev-quoted))
		  (test (op assignment?) (reg exp))
		  (branch (label ev-assignment))
		  (test (op definition?) (reg exp))
		  (branch (label ev-definition))
		  (test (op if?) (reg exp))
		  (branch (label ev-if))
		  (test (op lambda?) (reg exp))
		  (branch (label ev-lambda))
		  (test (op begin?) (reg exp))
		  (branch (label ev-begin))
		  (test (op cond?) (reg exp))
		  (branch (label ev-cond))
		  (test (op let?) (reg exp))
		  (branch (label ev-let))
		  (test (op compile?) (reg exp))
		  (branch (label ev-compile))
		  (test (op application?) (reg exp))
		  (branch (label ev-application))
		  (goto (label unknown-expression-type))

					;简单表达式的求值
		  ev-self-eval
		  (assign val (reg exp))
		  (goto (reg continue))

		  ev-variable
		  (assign val (op lookup-variable-value) (reg exp) (reg env))
		  (goto (reg continue))

		  ev-quoted
		  (assign val (op text-of-quotation) (reg exp))
		  (goto (reg continue))

		  ev-lambda
		  (assign unev (op lambda-parameters) (reg exp))
		  (assign exp (op lambda-body) (reg exp))
		  (assign val
			  (op make-procedure) (reg unev) (reg exp) (reg env))
		  (goto (reg continue))

					;过程应用的求值
		  ev-application
		  (save continue)
		  (assign unev (op operands) (reg exp))
		  (assign exp (op operator) (reg exp))
		  (test (op variable?) (reg exp))
		  (branch (label ev-appl-var-operator))
		  (assign continue (label ev-appl-did-compound-operator))
		  (save env)
		  (save unev)
		  (goto (label eval-dispatch))

		  ev-appl-var-operator
		  (assign continue (label ev-appl-did-operator))
		  (goto (label eval-dispatch))

		  ev-appl-did-compound-operator
		  (restore unev)
		  (restore env)
		  (goto (label ev-appl-did-operator))
		  
		  ev-appl-did-operator
		  (assign argl (op empty-arglist))
		  (assign proc (reg val))
		  (test (op no-operands?) (reg unev))
		  (branch (label apply-dispatch))
		  (save proc)

		  ev-appl-operand-loop
		  (save argl)
		  (assign exp (op first-operand) (reg unev))
		  (test (op last-operand?) (reg unev))
		  (branch (label ev-appl-last-arg))
		  (save env)
		  (save unev)
		  (assign continue (label ev-appl-accumulate-arg))
		  (goto (label eval-dispatch))

		  ev-appl-accumulate-arg
		  (restore unev)
		  (restore env)
		  (restore argl)
		  (assign argl (op adjoin-arg) (reg val) (reg argl))
		  (assign unev (op rest-operands) (reg unev))
		  (goto (label ev-appl-operand-loop))

		  ev-appl-last-arg
		  (assign continue (label ev-appl-accum-last-arg))
		  (goto (label eval-dispatch))

		  ev-appl-accum-last-arg
		  (restore argl)
		  (assign argl (op adjoin-arg) (reg val) (reg argl))
		  (restore proc)
		  (goto (label apply-dispatch))

					;过程应用
		  apply-dispatch
		  (test (op primitive-procedure?) (reg proc))
		  (branch (label primitive-apply))
		  (test (op compound-procedure?) (reg proc))
		  (branch (label compound-apply))
		  (test (op compiled-procedure?) (reg proc))
		  (branch (label compiled-apply))
		  (goto (label unknown-procedure-type))

		  primitive-apply
		  (assign val
			  (op apply-primitive-procedure) (reg proc) (reg argl))
		  (restore continue)
		  (goto (reg continue))

		  ;;组合过程会改变当前环境，导致所有表达式对其各项求值前都要save好当前env
		  compound-apply
		  (assign unev (op procedure-parameters) (reg proc))
		  (assign env (op procedure-environment) (reg proc))
		  (assign env
			  (op extend-environment) (reg unev) (reg argl) (reg env))
		  (assign unev (op procedure-body) (reg proc))
		  (goto (label ev-sequence))

		  compiled-apply
		  (restore continue)
		  (assign val (op compiled-procedure-entry) (reg proc))
		  (goto (reg val))
		  
					;ch5.4.2
		  ev-begin
		  (assign unev (op begin-actions) (reg exp))
		  (save continue)
		  (goto (label ev-sequence))

		  ;;来到这里时，stack里只有起始continue，unev放着当前sequence，env是起始env
		  ev-sequence
		  (assign exp (op first-exp) (reg unev))
		  (test (op last-exp?) (reg unev))
		  (branch (label ev-sequence-last-exp))
		  (save unev)
		  (save env)
		  (assign continue (label ev-sequence-continue))
		  (goto (label eval-dispatch))

		  ev-sequence-continue
		  (restore env)
		  (restore unev)
		  (assign unev (op rest-exps) (reg unev))
		  (goto (label ev-sequence))

		  ;;看上去，对这一系列过程的调用就像只调用了最后一个过程一样。这就是尾递归
		  ev-sequence-last-exp
		  (restore continue)
		  (goto (label eval-dispatch))

					;尾递归
		  ;;以下的改动，在求值最后一个exp前，将继续将当前env和unev进栈，求值完成后才出栈
		  ;;一旦遇到递归函数，所需栈空间将正比于迭代次数。
		  ;; ev-sequence
		  ;; (test (op no-more-exps?) (reg unev))
		  ;; (label ev-sequence-end)
		  ;; (...)
		  
		  ;; ev-sequence-end
		  ;; (restore continue)
		  ;; (goto (reg continue))

					;ch5.4.3
		  ev-if
		  (save exp)
		  (save env)
		  (save continue)
		  (assign continue (label ev-if-decide))
		  (assign exp (op if-predicate) (reg exp))
		  (goto (label eval-dispatch))

		  ev-if-decide
		  (restore continue)
		  (restore env)
		  (restore exp)
		  (test (op true?) (reg val))
		  (branch (label ev-if-consequent))

		  ev-if-alternative
		  (assign exp (op if-alternative) (reg exp))
		  (goto (label eval-dispatch))

		  ev-if-consequent
		  (assign exp (op if-consequent) (reg exp))
		  (goto (label eval-dispatch))

					;赋值和定义
		  ev-assignment
		  (assign unev (op assignment-variable) (reg exp))
		  (save unev)
		  (assign exp (op assignment-value) (reg exp))
		  (save env)
		  (save continue)
		  (assign continue (label ev-assignment-1))
		  (goto (label eval-dispatch))

		  ev-assignment-1
		  (restore continue)
		  (restore env)
		  (restore unev)
		  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
		  (assign val (const ok))
		  (goto (reg continue))

		  ev-definition
		  (assign unev (op definition-variable) (reg exp))
		  (save unev)
		  (assign exp (op definition-value) (reg exp))
		  (save env)
		  (save continue)
		  (assign continue (label ev-definition-1))
		  (goto (label eval-dispatch))

		  ev-definition-1
		  (restore continue)
		  (restore env)
		  (restore unev)
		  (perform (op define-variable!) (reg unev) (reg val) (reg env))
		  (assign val (const ok))
		  (goto (reg continue))

					;ex5.23
		  ;; ev-cond
		  ;; (assign exp (op cond->if) (reg exp))
		  ;; (goto (label eval-dispatch))

		  ev-let
		  (assign exp (op let->combination) (reg exp))
		  (goto (label eval-dispatch))

					;ex5.24
		  ev-cond
		  (assign unev (op cond-clauses) (reg exp))
		  (save continue)
		  (goto (label ev-cond-sequence))

		  ;;每次回到这里时，栈里只有起始continue，unev放着当前clauses，env是起始env
		  ev-cond-sequence
		  (test (op no-more-exps?) (reg unev))
		  (branch (label ev-cond-end))
		  (assign exp (op first-exp) (reg unev))
		  (test (op cond-else-clause?) (reg exp))
		  (branch (label ev-cond-success))
		  (save unev)
		  (save env)
		  (assign exp (op cond-predicate) (reg unev))
		  (assign continue (label ev-cond-continue))
		  (goto (label eval-dispatch))

		  ev-cond-end
		  (restore continue)
		  (goto (reg continue))

		  ev-cond-continue
		  (restore env)
		  (restore unev)
		  (test (op true?) (reg val))
		  (branch (label ev-cond-success))
		  (assign unev (op rest-exps) (reg unev))
		  (goto (label ev-cond-sequence))

		  ev-cond-success
		  (assign unev (op first-exp) (reg unev))
		  (assign unev (op cond-actions) (reg unev))
		  (goto (label ev-sequence))

		  ev-compile
		  (assign exp (op compile-exp) (reg exp))
		  (assign val (op compile-and-assemble-in-eceval) (reg exp))
		  (goto (label external-entry))
		  )))
