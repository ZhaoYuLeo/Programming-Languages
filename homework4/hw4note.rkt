
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; f(n) = f(n-1) + f(n-2)
(define (fibonacci x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci (- x 1))
         (fibonacci (- x 2)))))

;; Takes a "count up" approach that remenbers previous answers. Takes
;; linear time.
(define (fibonacci2 x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
    (if (or (= x 1) (= x 2))
        1
        (f 1 1 3))))

;; f(n) = f(n-1) + f(n-2) ; Bottom-Up
;; count-stair-ways is actually fibonacci(CS61: disc04)
(define (count-stair-ways n)
  (letrec ([f (lambda (acc1 acc2 steps)
                (if (= steps n)
                    (+ acc1 acc2)
                    (f acc2 (+ acc1 acc2) (+ steps 1))))])
    (cond [(= n 1) 1]
          [(= n 2) 2]
          [(> n 2) (f 1 2 3)]
          [#t 0])))

;; f(n) = f(n-1) + f(n-2) + ... + f(n-k+1) + f(n-k). f(10, 3) = 274; f(4, 4) = 8
;; if k is greater than n, return 1. Bottom-Up
(define (count_k n k)
  (letrec ([add_first_k (lambda(xs k)
                          (if (or (null? xs) (= k 0))
                              0
                              (+ (car xs) (add_first_k (cdr xs) (- k 1)))))]
           [f (lambda (acc steps)
                (if (= steps n)
                    (add_first_k acc k)
                    (f (cons (add_first_k acc k) acc) (+ steps 1))))])
    (cond [(< n 0) 0]
          [(= n 0) 1]
          [(> n 0) (f (cons 1 (cons 0 null)) 1)])))

;; memoization
;; only use mutation when you do need it
(define fibonacci3
  (letrec([memo null]
          [f (lambda (x)
               (let ([ans (assoc x memo)])
                 (if ans
                     (cdr ans)
                     (let ([new-ans (if (or (= x 1) (= x 2))
                                        1
                                        (+ (f (- x 1))
                                           (f (- x 2))))])
                       (begin
                         (set! memo (cons (cons x new-ans) memo))
                         new-ans)))))])
    f))



;; copied
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))
 
;; Produce a infinite sequence. The first element of the sequence is a and the
;; second one is a + 1, third is a + 2, and so on.
(define (inc-num a)
  (lambda ()
    (cons a (inc-num (+ a 1)))))

;; What is the difference between unnecessary function wrapping and thunk
;; 1. unnecessary function wrapping :
;; use (lambda () (f)) instead of (f) ; same behavior with unnecessary wrapping.
;; 2. thunk :
;; use (lambda () e) instead of e ; delay evaluation.
;; using macro the expression will not be evaluated when you define either. But
;; e might be copied to anywhere you need directly and have to been evaluated
;; many times.

;; Make Adder Increasing(CS61: lab06)
;; Takes number a and returns a function takes number b returns a + b + c. The
;; first time been called, c is 0. Each time it is called again, c is increased
;; by 1. The second time it is called, return a + b + 1, then a + b + 2, the
;; third time, and so on.
(define (make-adder-inc a)
  (let ([c -1])
    (lambda (b) (begin
                  (set! c (+ c 1))
                  (+ a b c)))))

;; There is subtle difference between this and the stream we wrote in homework.
;; What it emphasizes is not a infinite sequence but the side effect generated
;; by the last function call.

;; How to implement the same thing without any mutation.
;; ((cdr ((cdr ((cdr ((stream-adder-inc 5)2)) 2)) 10)) 1)
(define (stream-adder-inc a)
  (lambda (b)
    (cons (+ a b) (stream-adder-inc (+ a 1)))))
