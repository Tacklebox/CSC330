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
    [(= n 0) (null)]
    [(> n 0) (
              let ([stream-item (s)]) (cons (car stream-item) (stream-for-n-steps (cdr stream-item) (- n 1)))
              )]
    [#t (error "Unexpected input")]
   ))

(define funny-number-stream null)

(define cat-then-dog null)

(define stream-add-zero null)


(define cycle-lists null)

(define vector-assoc null)

(define cached-assoc null)
