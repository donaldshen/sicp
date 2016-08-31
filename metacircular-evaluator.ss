					;update to ch4.1.4
(load "foundation-for-evaluator.ss")
					;求值器核心
					;eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp) (make-procedure (lambda-parameters exp)
				       (lambda-body exp)
				       env))
	((begin? exp) (eval-sequence (begin-actions exp) env))
	((cond? exp) (eval (cond->if exp) env))
	((and? exp) (eval (and->if exp) env))
	((or? exp) (eval (or->if exp) env))
	((let? exp) (eval (let->combination exp) env))
	((let*? exp) (eval (let*->nested-lets exp) env))
	((tagged-list? exp 'while) (eval (while->combination exp) env))
	((tagged-list? exp 'for) (eval (for->while exp) env))
	((application? exp) (apply2 (eval (operator exp) env)
				   (list-of-values (operands exp) env)))
	(else (error "Unknown expression type -- EVAL" exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))
(define (apply2 procedure arguments)
  (cond ((primitive-procedure? procedure)
	 (apply-primitive-procedure procedure arguments))
	((compound-procedure? procedure)
	 (eval-sequence (procedure-body procedure)
			(extend-environment (procedure-parameters procedure)
					    arguments
					    (procedure-environment procedure))))
	(else (error "Unknown procedure type -- APPLY2" procedure))))


					;assignment
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
		       (eval (assignment-value exp) env)
		       env)
  'assignment-done!)
					;definition
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    ;;what happen? // the word 'define' make this bug happen for no reason
    (eval (definition-value exp) env)
    env)
  'definition-done!)

					;if
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))
					;begin
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
	(else (eval (first-exp exps) env)
	      (eval-sequence (rest-exps exps) env))))

					;driver-loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")


(define the-global-environment (setup-environment))

(driver-loop)
