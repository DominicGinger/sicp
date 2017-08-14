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

