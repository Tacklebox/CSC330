;; Programming Languages, Homework 5 version 1.1
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs{{{
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body)
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct closure (env fun) #:transparent)
#| }}} |#

;; Problem A {{{

(define (mupllist->racketlist lst)
  (letrec
    ([mupllist? (lambda (possible-mupllist)
                  (cond
                    [(aunit? possible-mupllist) #t]
                    [(apair? possible-mupllist) (mupllist? (apair-e2 possible-mupllist))]
                    [else #f]
                  ))])
    (cond
      [(aunit? lst) null]
      [(apair? lst) (cons (if (mupllist? (apair-e1 lst)) (mupllist->racketlist (apair-e1 lst)) (apair-e1 lst)) (mupllist->racketlist (apair-e2 lst)))]
      [else lst]
      )))

(define (racketlist->mupllist lst)
  (cond
    [(empty? lst) (aunit)]
    [(pair? lst) (apair (if (list? (car lst)) (racketlist->mupllist (car lst)) (car lst)) (racketlist->mupllist (cdr lst)))]
    [else lst]
    ))
#| }}} |#

;; Problem B

(define (envlookup env str)#| {{{ |#
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))#| }}} |#

(define (extend-env env sym val)
  (cons (cons sym val) env))

(define (eval-under-env e env)
  (cond
    [(add? e)#| {{{ |#
     (let ([v1 (eval-under-env (add-e1 e) env)]
           [v2 (eval-under-env (add-e2 e) env)])
       (if (and (int? v1)
                (int? v2))
         (int (+ (int-num v1)
                 (int-num v2)))
         (error "MUPL addition applied to non-number")))]#| }}} |#
    [(ifgreater? e)#| {{{ |#
     (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
           [v2 (eval-under-env (ifgreater-e2 e) env)]
           [v3 (thunk (eval-under-env (ifgreater-e3 e) env))]
           [v4 (thunk (eval-under-env (ifgreater-e4 e) env))])
       (if (and (int? v1)
                (int? v2))
         (if (> (int-num v1) (int-num v2)) (v3) (v4))
         (error "MUPL isgreater first two arguments must be numbers")))]#| }}} |#
    [(isaunit? e)#| {{{ |#
     (let ([u (eval-under-env (isaunit-e e) env)])
       (if (aunit? u) (int 1) (int 0)))]#| }}} |#
    [(fst? e) (let ([p (eval-under-env (fst-e e) env)])#| {{{ |#
       (if (apair? p) (eval-under-env (apair-e1 p) env) (error "MUPL Type Error: fst applied to non-pair")))]#| }}} |#
    [(snd? e) (let ([p (eval-under-env (snd-e e) env)])#| {{{ |#
       (if (apair? p) (eval-under-env (apair-e2 p) env) (error "MUPL Type Error: snd applied to non-pair")))]#| }}} |#
    [(apair? e)#| {{{ |#
     (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]#| }}} |#
    [(aunit? e) e]
    [(int? e) e]
    #| B2 test |#
    [(closure? e) e]
    [(fun? e) (closure env e)]
    [(call? e)
     (let ([clo (eval-under-env (call-funexp) env)]
           [arg (eval-under-env (call-actual) env)])
       (if (not (closure? clo))
         (error "called something that wasnt a function")
         (let ([cfun (closure-fun clo)]
               [cenv (closure-env clo)])
           (eval-under-env
           (fun-body cfun)
           (extend-env
             (if (fun-nameopt cfun)
               (extend-env cenv (fun-nameopt cfun) clo)
               cenv)
             (fun-formal cfun) arg)))))]
    [(mlet? e)#| {{{ |#
     (eval-under-env
       (mlet-body e)
       (extend-env env (mlet-var e) (mlet-e e)))]#| }}} |#
    [(var? e)#| {{{ |#
     (envlookup env (var-string e))]#| }}} |#
    [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) "CHANGE")

(define (mlet* lstlst e2) "CHANGE")

(define (ifeq e1 e2 e3 e4) "CHANGE")

;; Problem D

(define mupl-map "CHANGE")
;; this binding is a bit tricky. it must return a function.
;; the first two lines should be something like this:
;;
;;   (fun "mupl-map" "f"    ;; it is  function "mupl-map" that takes a function f
;;       (fun #f "lst"      ;; and it returns an anonymous function
;;          ...
;;
;; also remember that we can only call functions with one parameter, but
;; because they are curried instead of
;;    (call funexp1 funexp2 exp3)
;; we do
;;    (call (call funexp1 funexp2) exp3)
;;

(define mupl-mapAddN
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))
