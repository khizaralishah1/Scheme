#lang scheme
;Question 1: Exercise 1.30 of the book
(define (sum term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))

  (iter a 0))

(define (cubes x) (* x x x))
(define (inc n) (+ n 1))

(sum cubes 1 inc 4)

;Question 2: Exercise 1.41 of the book.
;Define a procedure double that takes a pro- cedure
;of one argument as argument and returns a proce- dur
;e that applies the original procedure twice.
;For exam- ple, if inc is a procedure that adds 1
;to its argument, then (double inc) should be a procedure
;that adds 2. What value is returned by

;(define (double proc)
;  (proc proc))
;
;((double inc) 5)

;(((double (double double)) inc) 5)
;Question 3: Exercise 2.18 of the book.
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2)))
  )

(define (reverses ls)
  (cond ((null? (cdr ls)) ls)
        (else (append (reverses (cdr ls)) (list (car ls))))))

(reverses (list 1 2 3 4 5 6 7))

;Question 4: Exercise 2.31 of the book.
(define (square x) (* x x))

(define (square-tree ls)
  (cond ((null? ls) ls)
      ((pair? (car ls)) (list (square-tree (car ls)) (square-tree (cdr ls))))
      (else (cons (square (car ls)) (square-tree (cdr ls))))
      )
  )

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define (square-tree-map tree square)
  (map (lambda (sub-tree)
         ( if (pair? sub-tree) (square-tree-map sub-tree square)
              (square sub-tree)
                           )) tree)
  )

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) square)

;Question 5: Exercise 2.32 of the book.

