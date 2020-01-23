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


(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
        [ #t (+ (sum2 (car xs)) (sum2 (cdr xs)))]))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs)) (car xs)]
        [#t (let ([tlans (max-of-list (cdr xs))])
              (if (> tlans (car xs))
                  tlans
                  (car xs)))]))

; STREAMS
; 1 1 1 1 1 ...
(define ones (lambda () (cons 1 ones)))
; 1 2 3 4 5 ...
(define (f x) (cons x (lambda () (f (+ x 1)))))
; again
; (define nats (lambda () (f 1)))
; again
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 1 2 4 8 16
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 1))))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f 1))))

(define nats2 (stream-maker + 1))
(define powers2 (stream-maker * 2))

; Macros
