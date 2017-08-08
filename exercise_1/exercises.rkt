;; Exercise 1.1
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; 3
(define b (+ a 1)) ;; 4
(+ a b (* a b)) ;; 19
(= a b) ;; f
(if (and (> b a) (< b (* a b)))
  b
  a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ;; -37/150

;; Exercise 1.3
(define (square x) (* x x))
(define (sum-square-big x y z)
  (cond ((and (< x y) (< x z)) (+ (square y) (square z)))
        ((and (< y x) (< y z)) (+ (square x) (square z)))
        ((and (< z x) (< z y)) (+ (square x) (square y)))))
(sum-square-big 19 23 21) ;; 970

;; Exercise 1.7
(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess) (average guess (/ x guess)))
  (define (good-enough? last-guess guess) (< (abs (- last-guess guess)) 0.01))
  (define (sqrt-iter last-guess guess)
    (if (good-enough? last-guess guess)
      guess
      (sqrt-iter guess (improve guess))))
  (sqrt-iter 0.0 1.0))
(sqrt 9)

;; Exercise 1.8
(define (cubert x)
  (define (square a) (* a a))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess))
       3))
  (define (good-enough? last-guess guess) (< (abs (- last-guess guess)) 0.01))
  (define (cubert-iter last-guess guess)
    (if (good-enough? last-guess guess)
      guess
      (cubert-iter guess (improve guess))))
  (cubert-iter 0.0 1.0))
(cubert 27)

;; Exercise 1.11
;; recursive
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))
(f 6)

;; iterative
(define (f n)
  (define (f-iter a b c count)
    (if (< count 3)
      c
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (f-iter 0 1 2 n))
(f 4)

;; Exercise 1.12
(define (pascal x y)
  (if (or (= x 1) (= x y))
    1
    (+ (pascal (- x 1) (- y 1))
       (pascal x (- y 1)))))
(pascal 3 5)

;; Exercise 1.13
(define (sqrt x)
  (define (average x y) (/ (+ x y) 2))
  (define (improve guess) (average guess (/ x guess)))
  (define (good-enough? last-guess guess) (< (abs (- last-guess guess)) 0.01))
  (define (sqrt-iter last-guess guess)
    (if (good-enough? last-guess guess)
      guess
      (sqrt-iter guess (improve guess))))
  (sqrt-iter 0.0 1.0))

(define (pow x pwr)
  (cond ((<= pwr 0) 0)
        ((<= pwr 1) x)
        ((> pwr 1) (* x (pow x (- pwr 1))))))
        
(define (fib n)
  (define (fib a b n)
    (if (= n 0)
      a
      (fib b (+ a b) (- n 1))))
  (fib 0 1 n))

(define (calc-fib n)
  (/ (pow (/ (+ 1 (sqrt 5)) 2) n) (sqrt 5)))

(fib 10)
(calc-fib 10)

;; Exercise 1.16
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt (square b) (/ n 2)))
        (else (* b (fast-expt b (- n 1))))))
(fast-expt 2 8)

;; Exercise 1.17
(define (fast-multi x y)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= y 0) 0)
        ((even? y) (double (fast-multi x (halve y))))
        (else (+ x (fast-multi x (- y 1))))))

(fast-multi 31 300)

;; Exercise 1.18
(define (fast-multi x y)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (fast-multi-iter x y z)
    (cond ((= y 0) z)
          ((even? y) (fast-multi-iter (double x) (halve y) z))
          (else (fast-multi-iter x (- y 1) (+ x z)))))
  (fast-multi-iter x y 0))
(fast-multi 31 300)

;; Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q))
                   (+ (* q q) (* 2 q p))
                   (/ count 2)))
        (else (fib-iter (abs (+ (* b q) (* a q) (* a p)))
                        (abs (+ (* b p) (* a q)))
                        p
                        q
                        (- count 1)))))
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)

;; Exercise 1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

(define (prime? n)
  (= n (smallest-divisor n)))

;; Exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (current-milliseconds) start-time))
    (newline)
    ))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(timed-prime-test 2011309)

(define (search-for-primes n count)
  (if (= count 0)
    (values)
    (if (prime? n)
      (begin
        (newline)
        (display n)
        (search-for-primes (+ n 1) (- count 1)))
      (search-for-primes (+ n 1) count))))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)


;; Exercise 1.23

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next x)
  (if (= x 2) 3 (+ x 2)))

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

