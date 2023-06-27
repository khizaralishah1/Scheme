#lang scheme
;Khizar Ali Shah 2020196 SICP Assignment-3
;Sorry for submitting super-late (at least I didn't cheat)

(display "Question 1: Exercise 1.30 of the book\n")
(define (sum term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))

  (iter a 0))

(define (cubes x) (* x x x))
(define (inc n) (+ n 1))

(sum cubes 1 inc 4)
;---------------------------------------------------
(display "Question 2: Exercise 1.41 of the book.\n")

(define (double proc)
  (lambda (x) (proc (proc x))))

((double inc) 6)
(((double (double double)) inc) 5)

;k = (double double)
;k = (lambda (x) (double (double x))) runs 4 times
;m = (double k)
;m = (lamdda (x) (..4 times.. (..4..times x)))
;n = ((m inc) 5)
;increment is applied 16 times to 5, resulting in 21

;---------------------------------------------------
(display "Question 3: Exercise 2.18 of the book.\n")
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2)))
  )

(define (reverses ls)
  (cond ((null? (cdr ls)) ls)
        (else (append (reverses (cdr ls)) (list (car ls))))))

(reverses (list 1 2 3 4 5 6 7))

;---------------------------------------------------
(display "Question 4: Exercise 2.31 of the book.\n")

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
              (square sub-tree))) tree)
  )

(square-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) square)

;---------------------------------------------------

(display "Question 5: Exercise 2.32 of the book.\n")

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (put-first-elem (car s) rest)))))

;make subsets of the set excluding the first element
;and then append the first element

(define (put-first-elem elem lst)
  (if (null? lst)
      '()
      (cons (cons elem (car lst)) (put-first-elem elem (cdr lst)))))

(define s '(1 2 3))

(display (subsets s))
