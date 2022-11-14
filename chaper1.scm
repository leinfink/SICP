;; ### Chapter 1.1 ###


;; --- 1.1 ---

10 ; 10
(+ 5 3 4) ; 12
(- 9 1) ; 8
(/ 6 2) ; 3
(+ (* 2 4) (- 4 6)) ; 6

(define a 3) ; a
(define b (+ a 1)) ; b
(+ a b (* a b)) ; 19
(= a b) ; #f

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


;; --- 1.2 ---

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (/ 3 (- 6 2) (- 2 7)))


;; --- 1.3 ---

(define (square a)
  (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (larger-squares-sum a b c)
  "Returns the sum of the squares of the two larger numbers."
  (cond ((or (<= a b) (<= a c)) (sum-of-squares b c))
        ((or (<= b a) (<= b c)) (sum-of-squares a c))
        (else (sum-of-squares a b))))

;; --- 1.4 ---

;; addition for positive b, else subtraction
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


;; --- 1.5 ---

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; (test 0 (p))

;; (test 0 (p)) would loop indefinitely in applicative-order evaluation (as in Scheme)
;; since the argument gets evaluated when the function is called,
;; not only once used in the body.
;; in normal-order eval, y would never get evaluated
;; because the if-condition is true. test would return 0.


;; --- Example: Square Roots by Newtons's Method ---

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

(square (sqrt 0.001)) ; 0.00170... not perfect yet


;; --- 1.6 ---

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x) x)))

;; since new-if is not a special form (unlike if),
;; all the arguments get evaluated, so sqrt-iter gets called indefinitely


;; --- 1.7 ---

(define (sqrt-iter-better guess x old_guess)
  (if (< (/ (abs (- guess old_guess)) guess)
         1/10000)
      guess
      (sqrt-iter-better (improve guess x) x guess)))

(define (sqrt-better x)
  (sqrt-iter-better 1.0 x 2.0)) ; 2.0 just so the first "change" is 1

(square (sqrt-better 0.001)) ; 0.001000000000000034 -- pretty good!


;; ---1.8 ---

(define (cube a)
  (* a a a))

(define (cbrt-iter guess x)
  (if (good-enough-cbrt? guess x)
      guess
      (cbrt-iter (improve-cbrt guess x) x)))

(define (good-enough-cbrt? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cbrt guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 27) ; 3.0000005410641766 -- seems ok.



;; ### Chapter 1.2 ###

(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

(define (factorial-iterative n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


;; --- 1.9 ---

(define (+19a a b)
  (if (= a 0) b (inc (+19a (dec a) b)))) ;; recursive

(define (+19b a b)
  (if (= a 0) b (+19b (dec a) (inc b)))) ;; iterative


;; --- 1.10 ---

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;; TODO
