(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
	     balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))))

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((count 0))
    (define (dispatch pw m)
    ;;(lambda (pw m)
      (if (eq? pw password)
	  (begin (set! count 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       ((eq? m 'share) share)
		       (else (error "Unknown request -- MAKE-ACCOUNT" m))))
	  ;;normally dispatch will return a procedure, so it will report an error	 
	  ;;"Incorrect password"))
	  ;;the first thing in parenthesis will be considered as a procedure
	  (lambda (sth)
	    (display "Incorrect password")
	    (if (= count 6)
		"call-the-cops"
		(set! count (+ 1 count))))))
    (define (share another-pw)
      (lambda (pw m)
	(if (eq? pw another-pw)
	    (dispatch password m)
	    (dispatch 'doubi m))))
    dispatch))
;;))

					;ex3.1
(define (make-accumulator initial)
  (lambda (x)
    (set! initial (+ initial x))
    initial))
					;ex3.2
(define (make-monitored f)
  (let ((count 0))
    (lambda (m)
      (cond ((eq? m 'how-many-calls?) count)
	    ((eq? m 'reset-count) (set! count 0))
	    (else (set! count (+ count 1))
		  (f m))))))

(define a (make-account 10 'h))
					;ch3.1.2
(define random-init 1)
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= 1 (gcd (rand) (rand))))
					;return the probability of success of experiment
(define (monte-carlo trials experiment)
					;每次都调用同一个experiment
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
	  ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral p? x1 x2 y1 y2 trials)
  (* (abs (- x1 x2)) (abs (- y1 y2)) (monte-carlo trials
						  (lambda ()
						    (p? (random-in-range x1 x2)
							(random-in-range y1 y2))))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-pi2 trials)
  (exact->inexact (estimate-integral (lambda (x y)
				       (< (sqrt (+ (square x) (square y))) 1))
				     -1.0
				     1.0
				     -1.0
				     1.0
				     trials)))
					;ex3.6
(define new-rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) ((set! x (rand-update x))
				x))
	    ((eq? m 'reset) (lambda (new-value)
			      (set! x new-value)
			      x))
	    (else (error "Wrong message" m))))))

					;ch3.1.3
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

					;ex3.7
(define (make-joint account password new-pw)
  ((account password 'share) new-pw))

(define (another-make-joint account password new-pw)
  (lambda (pw m)
    (if (eq? pw new-pw)
	(account password m)
	(account 'doubi m))))

					;ex3.8
(define f
  (let ((m 0))
    (lambda (num)
      (let ((last-value m))
	(set! m num)
	last-value))))

(define another-f
  (lambda (first-value)
    (set! another-f (lambda (any-value) 0))
    first-value))
