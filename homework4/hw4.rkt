
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
;; takes time proportion to pos(n % len(xs)).
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
  (letrec ([f (lambda (x)
                (cons (if (= (remainder x 5) 0) (- x) x)
                      (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;; Problem6 : A stream where the elements of it alternate between the strings
;; "dan.jpg" and "dog.jpg" (starting with "dan.jpg").
(define dan-then-dog
  (letrec ([f (lambda ()
                (cons "dan.jpg" (lambda () (g))))]
           [g (lambda ()
                (cons "dog.jpg" (lambda () (f))))])
    (lambda () (f))))

;; Problem7 : Takes a stream s and returns another stream. If s would produce
;; v for its ith element, then (stream-add-zero s) would produce the pair
;; (0 . v) for its ith element.
(define (stream-add-zero s)
  (let ([pr (s)])
    (lambda () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

;; Problem8 : Takes two lists xs and ys and returns a stream. They mayn't have
;; the same length, but assume they are both non-empty. The elements producted
;; by the stream are pairs where the first part is from xs and the second part
;; is from ys. The stream cycles forever through the lists. (i.e. if xs is '(1
;; 2 3) and ys is '("a" "b"), then the stream would produce, (1 . "a"),(2 ."b")
;; , (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 . "b"), etc)
(define (cycle-lists xs ys)
  (letrec ([pxs xs]
           [pys ys]
           [f (lambda (xs ys)
                (cons (cons (car xs) (car ys)); Assume both non-empty
                      ; After traversing the list, replace it with the original one
                      (lambda () (f (if (null? (cdr xs)) pxs (cdr xs)); have to know when have gone through the list
                                    (if (null? (cdr ys)) pys (cdr ys))))))])
    (lambda () (f xs ys))))

;; Problem9 : Takesa value v and a vector vec. Return #f if no vector element is
;; a pair with a car field equal to v, else return the first pair with an equal
;; car field. Allows vector elements not to be pairs in which case it skips them
;; takes time proportinal to the pos of v.
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (n)
                (if (= n len)
                    #f
                    (let ([elem (vector-ref vec n)])
                      (cond [(not (pair? elem)) (f (+ n 1))]
                            [(equal? (car elem) v) elem]
                            [#t (f (+ n 1))]))))])
    (f 0)))

;; Problem10 : Takes a list xs and a number n and returns a function that takes
;; one argument v and returns the same thing that (assoc v xs) would return. Uses
;; an n-element cache of recent results with slots used in a round-robin fashion.
;; Assume n is positive.
(define (cached-assoc xs n)
  (let ([memo (make-vector n #f)]
        [pos 0])
    ; thanks to closure we can access the same memo without exposing it
    (lambda (v) (let ([ans (vector-assoc v memo)])
                  (or ans ; "anything other than false, counts as true"
                      (let ([new-ans (assoc v xs)])
                        ; we can never get ans from cache when new-ans is #f
                        ; why we did not record (cons v new-ans) but just discard it.
                        ; maybe because this case is rare and usually represents an error
                        ; there is no need to consume memory to record it.
                        (and new-ans
                             (begin
                               (vector-set! memo pos (cons v new-ans))
                               (set! pos (remainder (+ pos 1) n))
                               new-ans))))))))

;; Problem11 : Define a macro that is used like (while-less e1 do e2)
;; evaluates e1 exactly once, evaluates e2 at least once and keeps evaluating e2
;; until and only until the result is not a number less than the result of the
;; evaluation of e1. Assuming evaluation terminates, the result is #f. If what e1
;; and e2 produced are not numbers, the macro just fails.
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([result e1]
              [f (lambda () (if (< e2 result) (f) #t))])
       (f))]))
