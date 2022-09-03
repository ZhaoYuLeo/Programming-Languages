
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; produces a list of numbers from low to high(including low and possibly high) separated by stride and in sorted order
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))
