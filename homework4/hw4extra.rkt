
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

;; Problem5 : Takes in two streams s1 and s2 and returns a stream that produces
;; the pairs that result from the other two streams(so the first value for the
;; result stream will be the pair of the first value of s1 and the first value
;; of s2).
(define (stream-zip s1 s2)
  (let ([cur1 (s1)]
        [cur2 (s2)])
  (lambda () (cons (cons (car cur1) (car cur2)) (stream-zip (cdr cur1) (cdr cur2))))))

;; Problem6 : stream-reverse that is like Racke's reverse function but works on streams
;; we can not reverse an infinite sequence

;; Problem7 : Takes a list of streams and produces a new stream that takes one
;; element from each stream in sequence. So it will first produce the first value
;; of the first stream, then the first value of the second stream and so on, and
;; it will go back to the first stream when it reaches the end of the list. Try
;; to do this without ever adding an element to the end of a list.
(define (interleave streams)
  (letrec ([f (lambda (streams next-state)
             (if (null? streams)
                 (f (reverse next-state) null); O(n)
                 (let* ([s (car streams)]
                        [value (s)])
                   (cons (car value) (lambda () (f (cdr streams) (cons (cdr value) next-state)))))))])
  (lambda () (f streams null))))

;; Problem8 : takes an integer n and a stream s, and returns a stream that produces
;; the same values as s but packed in lists of n elements. The first value of the new
;; stream will be the list consisting of the first n values of s.
(define (pack n s)
  (letrec ([first-s s]
           [f (lambda (n s)
                (if (= n 0)
                    (begin (set! first-s s) null); products lists and records s
                    (let ([value (s)])
                      (cons (car value) (f (- n 1) (cdr value))))))]
           [g (lambda () (cons (f n first-s) (lambda () (g))))])
    (lambda () (g))))

;; Problem9 : Takes a number n, starts with n as an initial guess in the stream, and
;; produces successive guesses applying fn(x) = 1/2(x + n/x) to the current guess.
;; Newton's Method for approximating the square root of a number, but by producing a
;; stream of every better approximation.
(define (sqrt-stream n)
  (letrec ([f (lambda (x)
                (let ([cur-guess (/ (+ x (/ n x)) 2.0)])
                  (cons cur-guess (lambda () (f cur-guess)))))])
    (lambda () (f n))))

;; Problem10 : Takes two numbers n and e and returns a number x such that x⋅x is within
;; e of n.
(define (approx-sqrt n e)
  (letrec ([float-n (* n 1.0)]
           [float-e (* e 1.0)]
           [within-e (lambda (x) (< (abs (- float-n (* x x))) float-e))]
           [until (lambda (s f)
                    (let ([value (s)])
                      (if (f (car value))
                          (car value)
                          (until (cdr value) f))))])
    (until (sqrt-stream n) within-e)))