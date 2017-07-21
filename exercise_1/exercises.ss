; Exercise 1.1
10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6
(define a 3) ; 3
(define b (+ a 1)) ; 4
(+ a b (* a b)) ; 19
(= a b) ; f
(if (and (> b a) (< b (* a b)))
    b
    a) ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; 16

; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ; -37/150

; Exercise 1.3
(define (square x) (* x x))
(define (sum-square-big x y z)
  (cond ((and (< x y) (< x z)) (+ (square y) (square z)))
        ((and (< y x) (< y z)) (+ (square x) (square z)))
        ((and (< z x) (< z y)) (+ (square x) (square y)))))
(sum-square-big 19 23 21) ; 970

; Exercise 1.7
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

; Exercise 1.8
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

