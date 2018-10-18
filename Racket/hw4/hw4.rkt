#lang racket
; Question 1
(define (sequence low high stride)
  (if (< low high) (cons low (sequence (+ low stride) high stride)) null))
; tests
; (sequence 3 11 2) -> `(3 5 7 9 11)
; (sequence 3 8 3) -> `(3 6)
; (sequence 3 2 1) -> `()

; Question 2
(define (string-append-map xs suffix)
  (map (lambda (element) (string-append element suffix)) xs))
; tests
; (string-append-map `("Beer" "Soda" "Milk") " is my favourite drink.") -> `("Beer is my favourite drink." "Soda is my favourite drink." "Milk is my favourite drink.") 

