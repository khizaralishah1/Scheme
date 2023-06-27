#lang scheme

#(A 1 5)
#(A 0 (A 1 4))
#(A 0 (A 0 (A 1 3)))
#(A 0 (A 0 (A 0 (A 1 2))))
#(A 0 (A 0 (A 0 (A 0 (A 1 1)))))
#(A 0 (A 0 (A 0 (A 0 2))))
#(A 0 (A 0 (A 0 4)))
#(A 0 (A 0 8))
#(A 0 16)
#(32)

#(A 2 3)
#(A 1 (A 2 2))
#(A 1 (A 1 (A 2 1)))
#(A 1 (A 1 (A 1 (A 2 0))))
#(A 1 (A 1 (A 1 0)))
#(A 1 (A 1 0))
#(A 1 0)
#(0)


#(0 1 2 4 11 25 59)

(define (f n)
   (if (< n 3) n
   (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f1 n)
 (define (f2 ctr a b c)
   (if (= ctr n) c
   (f2 (+ ctr 1) b c (+ c (* 2 b) (* 3 a)))))

  (if (< n 3) n
      (f2 2 0 1 2)))

(define (mult a b)
  (define (mult2 a b ctr)
    (cond ((even? b) (mult2 (* a 2) (/ b 2) ctr))
      ((> b 1) (mult2 a (- b 1) (+ ctr a)))
      (else (+ a ctr))))
  (mult2 a b 0)
  )
#(5 11 0)
#(5 11 0)

(define (mult3 a b)

  (cond ((even? b) (mult3 (* a 2) (/ b 2)))
        ((> b 1) (+ a (mult3 a (- b 1))))
        (else a)))


;-------|QUESTION 5|-------
;I didn't understand the working of fast exponentiation, how the "a^n mod N"
;works by reducing 'a' in a squared manner. I read about it from the book
;'Algorithms by S. Dasgupta, C. H. Papadimitriou, and U. V. Vazirani',
;but haven't grasped the core idea yet. However, I understand that when it
;is implemented, it can perform "a^n modulo N" in polynomial time.



