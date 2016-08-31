					;ex5.2
(controller
 test-c
 (test (op >) (reg c) (reg n))
 (branch (label factorial-done))
 (assign t1 (op *) (reg c) (reg p))
 (assign t2 (op +) (reg c) (const 1))
 (assign p (reg t1))
 (assign c (reg t2))
 (goto (label test-c))
 factorial-done)
					;ex5.3
(controller
 test-g
 (test (op good-enough?) (reg guess) (reg x))
 (branch (label sqrt-done))
 (assign guess (op improve) (reg guess) (reg x))
 (goto (label test-g))
 sqrt-done
 )

(controller
 (assign t (op -) (reg a) (reg b))
 test-t
 (test (op <) (reg t) (reg b))
 (branch (label gcd-done))
 (assign t (op -) (reg t) (reg b))
 (goto (label test-t))
 gcd-done
 )
					;ch5.1.4
(controller
 (assign continue (label fact-done))
 
 fact-loop
 (test (op =) (reg n) (const 1))
 (branch (label base-case))
 (save continue)
 (save n)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-fact))
 (goto (label fact-loop))

 after-fact
 (restore n)
 (restore continue)
 (assign val (op *) (reg n) (reg val))
 (goto (reg continue))

 base-case
 (assign val (const 1))
 (goto (reg continue))

 fact-done)

(controller
 (assign continue (label fib-done))

 fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label immediate-answer))
 (save continue)
 (assign continue (label afterfib-n-1))
 (save n)
 (assign n (op -) (reg n) (const 1))
 (goto (label fib-loop))

 afterfib-n-1
 (restore n)
 (assign n (op -) (reg n ) (const 2))
 (assign continue (label afterfib-n-2))
 (save val)
 (goto (label fib-loop))

 afterfib-n-2
 (assign n (reg val))
 (restore val)
 (restore continue)
 (assign val (op +) (reg val) (reg n))
 (goto (reg continue))

 immediate-answer
 (assign val (reg n))
 (goto (reg continue))

 fib-done)

;;a					;ex5.4
(controller
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

 expt-done)
;;b
(controller
 (assign counter (reg n))
 (assign product (const 1))

 expt-iter
 (test (op =) (reg counter) (const 0))
 (branch (label expt-done))
 (assign counter (op -) (reg counter) (const 1))
 (assign product (op *) (reg b) (reg product))
 (goto (label expt-iter))

 expt-done)
