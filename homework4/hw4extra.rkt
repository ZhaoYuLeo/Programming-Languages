
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem1 : Takes a list of numbers and evaluates to a list of numbers
;; of the same length, where each element is obtained as follows: the
;; first element should be the sum of the first and the last elements of
;; the original list, the second one should be the sum of the second and
;; second to last elements of the original list,etc. (i.e. (palindromic
;; (list 1 2 4 8)) evaluates to (list 9 6 6 9)).
(define (palindromic xs)
  (letrec ([invert (lambda (xs acc)
                     (if (null? xs)
                         acc
                         (invert (cdr xs) (cons (car xs) acc))))]
           ; invariant: xs has same length with ys
           [sum-map (lambda (xs ys acc)
                      (if (null? xs)
                          acc
                          (sum-map (cdr xs) (cdr ys)
                                   (cons (+ (car xs) (car ys)) acc))))])
    (sum-map xs (invert xs null) null)))

;; Problem2 : This is a stream, the first element of which is 0, the second
;; one is 1, and each successive element is the sum of two immediately pre-
;; ceding elements.
;; the key point of stream is thunk
(define fibonacc
         (letrec ([s (lambda (acc1 acc2)
                       (cons (+ acc1 acc2) (lambda ()
                                             (s (+ acc1 acc2) acc1))))])
           (lambda () (cons 0 (lambda () (cons 1 (lambda () (s 1 0))))))))

;; Problem3 : Takes a function f and a stream s, and applies f to the values
;; of s in succession until f evaluates to #f.

;; The definition of this function is not quite clear. Should I return the
;; last element of the stream, or a stream which is exactly the same with the
;; original one but will stop when f evaluates to #f or a new stream where the
;; elements of it is (f s) and same with the second case, stop when f evaluate
;; to #f ? By definition, a stream is an infinite sequence of values. Maybe
;; I should return a list instead. This is very confusing.

;; Returns a list. The ith element of the list is the result of applying f to
;; the value of s in succession until f evaluates to #f.
(define (stream-until f s)
  (let* ([cur (s)]
        [result (f (car cur))])
    (if result
     (cons result (stream-until f (cdr cur)))
     null)))

;; Problem4 : Takes a function f and a stream s, and returns a new stream whose
;; values are the result of applying f to the values produced by S.
(define (stream-map f s)
  (let ([cur (s)])
    (lambda () (cons (f (car cur)) (stream-map f (cdr cur))))))