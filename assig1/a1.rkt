#lang scheme

;1

(define (twoEven a b c)
  (if (even? a)
      (if (or (even? b) (even? c))
          #t
          #f)
      (if (and (even? b) (even? c))
          #t
          #f)
   ))

(twoEven 1 2 3)
(twoEven 4 2 3)
(twoEven 1 2 8)
(twoEven 1 3 5)

;2

(define (tax salary)
  (cond ((< salary 20000) 0)
        ((and (>= salary 20000) (< salary 50000)) (* salary 0.1))
        ((and (>= salary 50000) (< salary 500000))(* salary 0.2))
        (* salary 0.3)
        ))

(tax 10000)
(tax 20000)
(tax 30000)
(tax 40000)
(tax 50000)
(tax 60000)

;3

(define (mod a b)
  (if (> (- a b) b)
      (mod (- a b) b)
      (if (= (- a b) b)
          0
          (- a b))
  )
)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

(gcd 25 15)
(gcd 36 6)
(gcd 21 9)
(gcd 48 16)

;4

(define (square x) (* x x))

(define (improves x y)
  (/ (+ (/ x (square y)) (* 2 y)) 3)
)

(define (goodenough x y)
  (if (< (abs (- x (* y (* y y)))) 0.01)
  #t
  #f))

(define (cr x y)(
  if(goodenough x y)
  y
  (cr x (improves x y))))

(define (cuberoot x)
  (cr x 1))

(cuberoot 125)
(cuberoot 46)
(cuberoot 210)

;5

(define (newif predicate then els)
  (cond (predicate then)
        (els)))

(define (average x y) (/ (+ x y) 2))

(define (improve guess x) (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (newif (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
;New-if is a procedure which opens and evaluates all its formal
;arguments first before entering the body. It means it evaluates
;'then' and 'else' arguments regardless of the truth value of
;the predicate. It may be a case that predicate value may be true
;and 'else' statement shouldn't run, but here it runs and causes
;infinite loop.