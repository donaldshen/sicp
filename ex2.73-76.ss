(define (add-complex z1 z2)
	(make-from-real-imag (+ (real-part z1) (real-part z2))
			     (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
	(make-from-real-imag (- (real-part z1) (real-part z2))
			     (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
	(make-from-mag-ang (* (magnitude z1) (magnitude z2))
			     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
	(make-from-mag-ang (/ (magnitude z1) (magnitude z2))
			     (- (angle z1) (angle z2))))

(define (install-rectangular-package)
	;internal procedures
	(define (real-part z) (car z))
	(define (imag-part z) (cdr z))
	(define (magnitude z) (sqrt (+ (square (real-part z)) (square (imag-part z)))))
	(define (angle z) (atan (imag-part z) (real-part z)))
	(define (make-from-real-imag x y) (cons x y))
	(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

	;interface to the rest of the system
	(put 'real-part '(rectangular) real-part)
	(put 'imag-part '(rectangular) imag-part)
	(put 'magnitude '(rectangular) magnitude)
	(put 'angle '(rectangular) angle)
	(define (tag x) (attach-tag 'rectangular x))
	(put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

(define (install-polar-package)
	;internal procedures
	(define (magnitude z) (car z))
	(define (angle z) (cdr z))
	(define (real-part z) (* (magnitude z) (cos a)))
	(define (imag-part z) (* (magnitude z) (sin a)))
	(define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))
	(define (make-from-mag-ang r a) (cons r a))

	;interface to the rest of the system
	(put 'real-part '(polar) real-part)
	(put 'imag-part '(polar) imag-part)
	(put 'magnitude '(polar) magnitude)
	(put 'angle '(polar) angle)
	(define (tag x) (attach-tag 'polar x))
	(put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
	(put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))
	'done)

(define (apply-generic op . args)
	(let ((type-tags (map type-tag args)))
	     (let ((proc (get op type-tags)))
	     	  (if proc
		      (apply proc (map contents args))
		      (error "No method for these types -- APPLY-GENERIC"
		      	     (list op type-tags))))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
	((pair? datum) (car datum))
	(else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(else (error "Bad tagged datum -- CONTENTS" datum))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang x y) ((get 'make-from-mag-ang 'polar) x y))

(define (deriv exp var)
	(cond ((number? exp) 0)
	      ((variable? exp) (if (same-variable? exp var) 1 0))
	      (else ((get 'deriv (operator exp)) (operands exp) var))))

(define (same-variable? v1 v2) (eq? v1 v2))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (make-sum x y)
  (cond ((= x 0) y)
	((= y 0) x)
	((and (number? x) (number? y)) (+ x y))
	(else (list '+ x y))))
(define (make-product x y)
  (cond ((or (= x 0) (= y 0)) 0)
	((= x 1) y)
	((= y 1) x)
	((and (number? x) (number? y)) (* x y))
	(else (list '* x y))))
 
(define (install-sum-package)
  (define (addend exp) (car exp))
  (define (augend exp) (cadr exp))

  (put 'deriv '+
       (lambda (operands var)
	 (make-sum (deriv (addend operands) var)
		   (deriv (augend operands) var)))))

(define (install-product-package)
  (put 'deriv '*
       (lambda (operands var)
	 (make-sum (make-product (car operands) (deriv (cadr operands) var)
				 (cadr operands) (deriv (car operands) var))))))

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
	((= exponent 1) base)
	((and (number? base) (number? exponent)) (expt base exponent))
	(else (list '** base exponent))))

(define (install-exponentiation-package)
  (put 'deriv '**
       (lambda (operands var)
	 (let ((base (car operands))
	       (exponent (cadr operands)))
	   (make-product exponent
			 (make-product (make-exponentiation base (- exponent 1))
				       (deriv base var)))))))

(define (make-from-mag-ang-message-version r a)
  (define (dispatch op)
    (cond ((eq? 'magnitude op) r)
	  ((eq? op 'angle) a)
	  ((eq? op 'real-part) (* r (cos a)))
	  ((eq? op 'imag-part) (* r (sin a)))
	  (else (error "Unknown op -- MAKE-FROM-MAG-ANG-MESSAGE-VERSION" op))))
  dispatch)
