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

(define (g n) (A 1 n)) ;; 2^n
;; (y0: 0, y1: 2, y2: (* 2 2) = 4, y3: (* 2 4), y4= (* 2 8), ...)

(define (h n) (A 2 n)) ;; 2^(2^n)
;; (y0: 0, y1: 2, y2: (A 1 2) = 4, y3: (A 1 (2^2)) = 2^4, 8, y4: (A 1 (2^3)) = 2^8, ...)


;; Fibonacci

(define (fib-rec n) ;; tree-recursive process: num of steps grows exponentially, space linerarly
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1))
                 (fib-rec (- n 2))))))

(define (fib-it n) ;; linear iterative process: num of steps grows linearly, space constant
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


;; Example: Counting change
;; writing a procedure that calculates how many different ways of changing x amount of money there are

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1) ;; no money counts as 1 way of changing it
        ((or (< amount 0) (= kinds-of-coins 0)) 0) ;; neg money or no coin-kinds is 0 ways of changing it
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))


;; --- 1.11 ---

(define (f11-rec n)
  (if (< n 3)
      n
      (+ (f11-rec (- n 1)) (* 2 (f11-rec (- n 2))) (* 3 (f11-rec (- n 3))))))

(define (f11-iter n)
  (define (iter i prev1 prev2 prev3)
    (if (= i (+ n 1))
        prev1
        (iter (+ i 1)
              (if (< i 3)
                  i
                  (+ prev1
                     (* 2 prev2)
                     (* 3 prev3)))
              prev1
              prev2)))
  (iter 0 0 0 0))


;; --- 1.12 ---

(define (pascal-triangle-element h x) ; h: height, x: steps from left
  (if (or (= x 1) (= x h))
      1
      (+ (pascal-triangle-element (- h 1) (- x 1))
         (pascal-triangle-element (- h 1) x))))


;; --- 1.13 ---

;; --- 1.14 ---

;; --- 1.15 ---

(define (cube x) (* x x x))
(define (p x)
  (display "hi")
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a: p is applied 5 times when (sine 12.15) is evaluated.
;; b: ...


;; Exponentiation

(define (expt-rec b n) ;; O(n) steps, O(n) space
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(define (expt-iter b n) ;; O(n) steps, O(1) space
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1)
              (* b product))))
  (iter n 1))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n) ;; O(log n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; --- 1.16 ---

(define (fast-expt-iter b n)
  (define (iter counter state)
    (cond ((= counter 0) 1)
          ((even? counter) (square ())))))
