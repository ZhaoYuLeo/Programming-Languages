
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; Problem 1: Produces a list of numbers from low to high(including low
;; and possibly high) separated by stride and in sorted order
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; Problem 2: Returns a list of strings. Each element of the output should
;; be the corresponding element of the input appended with suffix (with no
;; extra space between the element and suffix).
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; Problem 3: Returns the ith element of the given list where we count from
;; zero and i is the remainder produced when dividing n by the list's length
;; raise (error "list-nth-mod: negative number") when the given number is
;; negative raise (error "list-nth-mod: empty list") when the given list is
;; empty.
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;; Problem4: Takes a stream s and a number n and returns a list holding the
;; first n values produced by s in order. Assume n is non-negative.
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))


;; Problem5: Products a stream of natural numbers except numbers divisble by
;; 5 are negated like (1,2,4,-5,6,7,8,9,-10,11,...).
(define funny-number-stream
  (letrec ([f (lambda (x) (cons x (f (+ 1 x))))])
    (f 1)))