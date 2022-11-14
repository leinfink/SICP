;; 1.1

;; 1.2

;; 1.3

(define (square a)
  (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (fun-13 a b c)
  "Returns the sum of the squares of the two larger numbers."
  (cond ((or (<= a b) (<= a c)) (sum-of-squares b c))
        ((or (<= b a) (<= b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

;; 1.4

;; 1.5

;; Square Root

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; 1.6

;; 1.7

(define (sqrt-iter-17 guess x old_guess)
  (if (< (/ (abs (- guess old_guess))
            old_guess)
         1/1000)
      guess
      (sqrt-iter-17 (improve guess x) x guess)))

(define (sqrt-17 x)
  (sqrt-iter-17 1.0 x 2.0))

;; 1.8

(define (cbrt-iter guess x)
  (if (good-enough-cbrt? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x) x)))

(define (good-enough-cbrt? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube a)
  (* a a a))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cbrt x)
  (cbrt-iter 1.0 x))

;; ch. 1.2


(define factorial-lambda (lambda (n)
                           (if (= n 1)
                               1
                               (* n (factorial (- n 1))))))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial2 n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1 n))


;; 1.9

(define (+19a a b)
  (if (= a 0) b (inc (+19a (dec a) b)))) ;; recursive

(define (+19b a b)
  (if (= a 0) b (+19b (dec a) (inc b)))) ;; iterative

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;;
