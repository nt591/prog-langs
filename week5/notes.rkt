#lang racket


(provide (all-defined-out))

;(define cube1
; (lambda (x)
;  (* x (* x x))))

;(cube1 3)

;(define cube2
; (lambda (x)
;  (* x x x)))

;(cube2 3)

;(define (cube3 x)
;  (* x x x))

;(cube3 3)

(define (pow1 x y)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))

(pow1 2 4)

(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))

(define three-to-the (pow2 3))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(sum (list 1 2 3 4 5))

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(my-append (list 1 2 3) (list 4 5 6))

; map

(define (my-map f xs)
  (if (null? xs)
      null
      (cons
       (f (car xs))
       (my-map f (cdr xs)))))

(define foo (my-map (lambda (x) (+ x 4)) (list 1 2 3)))

(define xs (list 5 6 7))
(define ys (list 4 (list 5 6 (list 9 10) 7)))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs)  (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))


