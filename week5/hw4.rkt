
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder (length xs) (+ n 1))))]
         ))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (x n)
                (if (= n 0)
                    null
                    (cons (car
                           (x)) (f (cdr (x)) (- n 1)))))])
    (f s n)))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (cons (cond [( = (remainder x 5) 0) (- x)]
                       [#t x])
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (b)
                (cons (if b
                          "dan.jpg"
                          "dog.jpg")
                      (lambda () (f (not b)))))])
    (lambda () (f #t))))

(define (stream-add-zero str)
  (define (f s)
    (cons
     (cons 0 (car (s)))
     (lambda () (f (cdr (s))))))
  (lambda () (f str)))

(define (cycle-lists xs ys)
  (define (f count)
    (cons 
     ; CONS the A element onto the B element for the first value
     (cons (list-ref xs (remainder count (length xs)))
           (list-ref ys (remainder count (length ys))))
     (lambda () (f (+ 1 count)))))
  (lambda () (f 0)))

(define (vector-assoc v vec)
  (define (get idx)
    (if (>= idx (vector-length vec))
         #f
         (if (and (pair? (vector-ref vec idx)) (= (car (vector-ref vec idx)) v))
             (vector-ref vec idx)
             (get (+ idx 1)))))
  (get 0))

(define (cached-assoc xs n)
  (letrec (
    [memo (make-vector n #f)]
    [count 0]
    [f (lambda (v)
          (if (vector-assoc v memo)
              (vector-assoc v memo)
              (begin
                ; get answer from xs
                ; stick into memo
                (let ([val (assoc v xs)])
                (vector-set! memo count val)
                (set! count (remainder (+ 1 count) n))
                val))))])
  f))
