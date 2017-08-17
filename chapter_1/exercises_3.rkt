(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (sum-squares a b)
  (sum square a inc b))

(define (cube x) (* x x x))
(define (sum-cubes a b)
  (sum cube a inc b))

(sum-squares 1 10)
(sum-cubes 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 10000))

;; Exercise 1.29

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))
(integral cube 0 1 0.01)

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (term k)
     (* (cond ((or (= k 0) (= k 1)) 1)
             ((= (remainder k 2) 0) 2)
             (else 4)) (yk k)))
  (define (add-one x) (+ x 1))
  (* (/ h 3) (sum term 0 add-one n)))
(simpsons-integral cube 0 1 1000)

;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
  (if (> a b)
    result
    (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31

;; recursive accumulator
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))
(define (inc x) (+ x 1))
(define (square x) (* x x))
(accumulate + 0 square 1 inc 5)

;; iterative accumulator
(define (accumulate combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
      res
      (iter (next a) (combiner res (term a))))))
(define (inc x) (+ x 1))
(define (square x) (* x x))
(accumulate + 0 square 1 inc 5)

;; Exercise 1.32

; recursive
(define (accumulate combiner null-value term a next b) 
  (if (> a b) null-value 
    (combiner (term a) (accumulate combiner null-value term (next a) next b)))) 

;iterative
(define (accumulate combiner null-value term a next b) 
  (define (iter a res) 
    (if (> a b) res 
      (iter (next a) (combiner res (term a))))) 
  (iter a null-value)) 

(define (sum term a next b) (accumulate + 0 term a next b)) 
(define (product term a next b) (accumulate * 1 term a next b)) 

(sum square 1 inc 10)

;; Exercise 1.33
(define (filtered-accumulate combiner null-value term a next b filter) 
   (define (iter a result) 
     (cond ((> a b) result) 
           ((filter a) (iter (next a) (combiner result (term a)))) 
           (else (iter (next a) result)))) 
   (iter a null-value)) 

;; Exercise 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;; Exercise 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (displayln guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define tolerance 0.00001)
(fixed-point (lambda (x)
               (/ (+ x (/ (log 1000.00) (log x))) 2)) 2)


