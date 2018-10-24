#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; these definitions are simply for the purpose of being able to run the tests
;; you MUST replace them with your solutions
;;

(define (sequence low high stride)
  (if (<= low high) (cons low (sequence (+ low stride) high stride)) null))
; tests
; (sequence 3 11 2) -> `(3 5 7 9 11)
; (sequence 3 8 3) -> `(3 6)
; (sequence 3 2 1) -> `()

(define (string-append-map xs suffix)
  (map (lambda (element) (string-append element suffix)) xs))
; tests
; (string-append-map `("Beer" "Soda" "Milk") " is my favourite drink.") ->
; `("Beer is my favourite drink."
;   "Soda is my favourite drink."
;   "Milk is my favourite drink.")


(define (list-nth-mod xs n)
  (cond
    [(negative? n) (error "list-nth-mod: negative number")]
    [(empty? xs)   (error "list-nth-mod: empty list")]
    [#t (car (list-tail xs (remainder n (length xs))))]
    ))

(define (stream-for-n-steps s n)
  (cond
    [(= n 0) null]
    [(> n 0) (
              let ([stream-item (s)])
              (cons (car stream-item) (stream-for-n-steps (cdr stream-item) (- n 1)))
              )]
    [#t (error "Unexpected input")]
    ))

(define (funny-number-stream)
  (letrec
    ([funny-number-stream-helper (lambda (n)
                                   (cons
                                     (if (= 0 (remainder n 5)) (* -1 n) n)
                                     (lambda () (funny-number-stream-helper (+ 1 n)))
                                     ))])
    (funny-number-stream-helper 1)
    ))

(define (cat-then-dog)
  (letrec
    ([cat-thunker (lambda ()
                    (cons "cat.jpg" dog-thunker))]
     [dog-thunker (lambda ()
                    (cons "dog.jpg" cat-thunker))])
    (cat-thunker)))

(define (stream-add-zero s)
  (let
    ([stream-item (s)])
    (lambda () (cons (cons 0 (car stream-item)) (stream-add-zero (cdr stream-item))))
    ))


(define (cycle-lists xs ys)
  (lambda ()
    (cons (cons (car xs) (car ys))
          (cycle-lists (append (cdr xs) (list (car xs))) (append (cdr ys) (list (car ys))))
          )))

(define (vector-assoc v vec [eqf equal?])
    (letrec
      ([vector-assoc-helper (lambda (index)
                              (cond
                                [(= index (vector-length vec)) #f]
                                [(eqf v (car (vector-ref vec index))) (vector-ref vec index)]
                                [#t (vector-assoc-helper (+ 1 index))]
                                ))])
      (vector-assoc-helper 0)
      ))

(define cached-assoc null)
