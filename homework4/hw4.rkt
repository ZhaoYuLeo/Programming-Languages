
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
  (car (list-tail xs (remainder n (length xs)))))