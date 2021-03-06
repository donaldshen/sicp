(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serializer-p . args)
	(mutex 'acquire)
	;;make sure it releases the mutex after finishing p
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      serializer-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire)))
	    ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))
(define (test-and-set! cell)
  (without-interrupts
   (if (car cell)
       true
       (begin (set-car! cell true)
	      false))))
					;ex3.47
(define (make-list sth n)
  (if (= n 0)
      '()
      (cons sth (make-list sth (- n 1)))))
;;base on mutex
(define (make-n-mutex n)
  (let ((mutex (make-mutex)))
    (define (the-mutexs m)
      (cond ((eq? m 'acquire)
	     (mutex 'acquire)
	     (set! n (- n 1))
	     (if (> n 0)
		 (mutex 'release)))
	    ((eq? m 'release)
	     ;;no need for without-interrupts here
	     (set! n (+ n 1))
	     (mutex 'release))))
    the-mutexs))
;;base on test-and-set!
(define (make-n-mutex2 n)
  ;;(list false) is important for using test-and-set!
  (let ((cells (make-list (list false) n)))
    (define (the-mutexs m)
      (cond ((eq? m 'acquire)
	     (if (test-all-and-set! cells)
		 (the-mutexs 'acquire)))
	    ((eq? m 'release)
	     (clear-all! cells))))
    the-mutexs))
(define (test-all-and-set! cells)
  (if (null? cells)
      true
      (if (test-and-set! (car cells))
	  (test-all-and-set! (cdr cells))
	  false)))
(define (clear-all! cells)
  (if (not (null? cells))
      (if (caar cells)
	  (clear! (car cells))
	  (clear-all! (cdr cells)))))
;;another version
(define (make-n-mutex3 n)
  (define (the-mutexs m)
    (cond ((eq? m 'acquire)
	   ;;(if (> n 0)
	   ;;(set! n (- n 1));test and set! must be atom manipulation
	   (if (test-and-set2! n)
	       (the-mutexs 'acquire)))
	  ((eq? m 'release)
	   (set! n (+ n 1)))))
  the-mutexs)
(define (test-and-set2! n)
  (without-interrupts
   (if (> n 0)
       (begin (set! n (- n 1))
	      false)
       true)))
					;ex3.48
(define (serialized-exchange account1 account2)
  (define (exchange a1 a2)
    (let ((diff (- (a1 'balance)
		   (a2 'balance))))
      ((a1 'withdraw) diff)
      ((a2 'deposit) diff)))
  (define (s-e a1 a2)
    (let ((serializer1 (a1 'serializer))
	  (serializer2 (a2 'serializer)))
      ((serializer2 (serializer1 exchange))
       a1
       a2)))
  (if (< (account1 'id) (account2 'id))
      (s-e account1 account2)
      (s-e account2 account1)))
(define (counter)
  (let ((init 0))
    (lambda ()
      (set! init (+ init 1))
      init)))
(define c1 (counter))
(define (make-account balance)
  (let ((id (c1))
	(balance-serializer (make-serializer)))
    (define (withdraw amount)
      (if (>= balance amount)
	  (set! balance (- balance amount))
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
	    ((eq? m 'deposit) deposit)
	    ((eq? m 'id) id)
	    ((eq? m 'serializer) balance-serializer)
	    (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))
