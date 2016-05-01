(use srfi-1)
(define (range str stp inc)
  (cond ((< str stp)
	 (cons str (range (+ str inc) stp inc)))
	(else '())))

;problem-1
(apply + (filter (lambda (x)
		   (or (integer? (/ x 3))
		       (integer? (/ x 5))))
		 (range 0 1000 1)))

;problem-2  linebreaks are separate programs
(define (fib-max max)
  (let loop ((a 1)(b 2)(c '()))
    (if (> a max) c (loop b (+ a b) (cons a c)))))
(apply + (filter even? (fib-max 4000000)))

(define (fib-max max)
  (let loop ((a 1)(b 2)(c '())))
    (if (> a max) c (loop b (+ a b) (if (even? a) (cons a c) c)))))
(apply + (fib-max 4000000))

(let loop ((a 1)(b 2)(c 0))
  (if (> a 4000000) c (loop b (+ a b) (if (even? a) (+ a c) c))))

(let fib ((a 1)(b 2)(c 0))
  (if (> a 4000000) c (fib b (+ a b) (if (even? a) (+ a c) c))))

(let fib ((a 1)(b 2))(if (< a 1000)(+ (if (even? a) a 0) (fib b (+ a b)))0)))

;problem-3
(define (prime? number)
  (let help ((test 2))
    (cond ((< test number)
	   (if (not (integer? (/ number test)))
	       (help (+ test 1))
	       #f))
	  (else #t))))

(define (factor number)
  (let loop ((test 2)
	     (number number)
	     (factors '()))
    (if (> number 1)
	(if (prime? test)
	    (if (integer? (/ number test))
		(loop test (/ number test) (cons test factors))
		(loop (+ test 1) number factors))
	    (loop (+ test 1) number factors))
	factors)))

;problem-4
(define (palindrome? number)
  (let ((it (string->list (number->string number))))
    (equal? it (reverse it))))

(define (permute func list1 list2)
  (map (lambda (x)
	 (map (lambda (y)
		(func x y))
	      list2))
       list1))

(call/cc (lambda (r)
	   (let ((l (permute * (range 100 1000 1) (range 100 1000 1))))
	     (map (lambda (x)
		    (if (palindrome? x)
			(r x)))
		  (sort (flatten l)) >))))

;problem-5
(let ((divisible-up-to
       (lambda (x n)
	 (let loop ((t 1))
	   (if (< t n)
	       (if (integer? (/ x t))
		   (loop (+ t 1))
		   #f)
	       #t)))))
  (let loop ((start 1))
    (cond ((divisible-up-to start 21)
	   (printf "~A~N" start))
	  (else (loop (+ start 1))))))

;problem-6
(let* ((numbers (range 1 11 1))
       (sum-squares (apply + (map (lambda (x) (* x x)) numbers)))
       (square-sums ((lambda (x) (* x x)) (apply + numbers))))
  (- sum-squares square-sums))

;problem-7
(define (prime? number)
  (let help ((test 2))
    (cond ((< test number)
	   (if (not (integer? (/ number test)))
	       (help (+ test 1))
	       #f))
	  (else #t))))

(let loop ((prime 1) (number 2))
  (if (and (eq? prime 10001)(prime? number)) number
      (if (prime? number)
	  (loop (+ prime 1) (+ number 1))
	  (loop prime (+ number 1)))))

;problem-8
