
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
  (letrec ([invert (lambda (xs acc) (if (null? xs)
                                   acc
                                   (invert (cdr xs) (cons (car xs) acc))))]
           [sum-map (lambda (xs ys acc) (if (null? xs); invariant: xs has same length with ys
                                          acc
                                          (sum-map (cdr xs) (cdr ys) (cons (+ (car xs) (car ys)) acc))))])
    (sum-map xs (invert xs null) null)))