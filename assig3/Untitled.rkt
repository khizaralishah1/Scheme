#lang scheme
;(define x (cons 1 2))
;(define y (cons 3 4))
;(define z (cons x y))

;(car x)

;(cdr x)

;(car (car z))

;(car (cdr z))

(define (mod a b)
  (if (> (- a b) b)
      (mod (- a b) b)
      (- a b)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (mod a b))))

;(define (make-rat n d) (
;  let ((g (gcd n d)))
;  (cons (/ n g) (/ d g))))

;(make-rat 5 3)

(define (num x)
  (car x))

(define (den x)
  (cdr x))

;(define (make-segment x1 y1 x2 y2)
;  (define (point1) (cons x1 y1))
;  (define (point2) (cons x2 y2))
;  (cons (point1) (point2))
;  )

;(make-segment 1 2 3 4)

;(define (cons x y)

;  (define (dispatch m)
;  (cond ((= m 0) x)
;        ((= m 1) y)))
;  dispatch)


;(define (car z) (z 0))

(define (make-interval a b) (cons a b))

(define in1 (make-interval 5 35))
(define in2 (make-interval 10 20))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(add-interval in1 in2)

(define (mult-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))
  ))

(mult-interval in1 in2)

(define (div-interval x y)
  (mult-interval x
                 (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y)))))

(div-interval in1 in2)

(lower-bound in1)

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(center in1)
(width in1)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))
  ))

(define list1 (list 1 2 3 4 5 6))
(define list2 (list 1 2 3 4 5 6))

list1

(append list1 list2)
(length list1)

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(last-pair (list 4 5 3 2))

(define (reverse list3)
  (if (null? list3) '()
      (append (reverse (cdr list3)) (list (car list3))))
  )

(display "hello\n")

(reverse (list 1 2 3 4 5))
;1 2 3 4 5
;append list3 (2 3 4 5)
;append list3 (2 3 4 5)

(define aa (list 1 2 3))
(define ab (list 4 5 6))

(append aa ab)
(list aa ab)

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
      ((not? (pair? tree)) (* tree factor))
      (else (cons (scale-tree (car tree) factor)
            (scale-tree (cdr tree) factor)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence)))
         (cons))))

















