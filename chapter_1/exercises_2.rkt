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

(define start (current-milliseconds))
(search-for-primes 1000 1000)
(search-for-primes 10000 1000)
(search-for-primes 100000 1000)
(search-for-primes 1000000 1000)
(define finish (current-milliseconds))
(display (- finish start))

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

;; Exercise 1.24
(define (square x) (* x x))
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (search-for-fast-primes n count)
  (if (= count 0)
    (values)
    (if (fast-prime? n 1)
      (begin
        (newline)
        (display n)
        (search-for-fast-primes (+ n 1) (- count 1)))
      (search-for-fast-primes (+ n 1) count))))

(define start (current-milliseconds))
(search-for-fast-primes 1000 1000)
(search-for-fast-primes 10000 1000)
(search-for-fast-primes 100000 1000)
(search-for-fast-primes 1000000 1000)
(define finish (current-milliseconds))
(newline)
(display (- finish start))

;; Exercise 1.27
(define (fooled n)
  (display (fermat-test n))
  (display " : ")
  (display (prime? n))
  (newline))

(fooled 561)  
(fooled 1105)  
(fooled 1729)  
(fooled 2465)  
(fooled 2821)  
(fooled 6601)  

;; Exercise 1.28
(define (square x) (* x x))
(define (expmod base exp m)
  (define (check-non-trivial x)
    (define y (remainder (square x) m))
    (if (and (= y 1) (not (= x 1)) (not (= x (- m 1))))
      0
      y))
  (cond ((= exp 0) 1)
        ((even? exp)
         (check-non-trivial (expmod base (/ exp 2) m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n) 
   (define (try-it a) 
     (define (check-it x) 
       (and (not (= x 0)) (= x 1))) 
     (check-it (expmod a (- n 1) n))) 
   (try-it (+ 1 (random (- n 1))))) 

(define (fooled n)
  (display (miller-rabin-test n))
  (display " : ")
  (display (prime? n))
  (newline))

(fooled 561)  
(fooled 1105)  
(fooled 1729)  
(fooled 2465)  
(fooled 2821)  
(fooled 6601)  
