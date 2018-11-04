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
  (if sym
    (cons (cons sym val) env)
    env))

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
         (error "MUPL ifgreater first two arguments must be numbers")))]#| }}} |#
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
    [(call? e)#| {{{ |# (let ([c (eval-under-env (call-funexp e) env)]
           [arg (eval-under-env (call-actual e) env)])
       (if (closure? c)
         (let* ([cfun (closure-fun c)]
                [cfname (fun-nameopt cfun)]
                [argsym (fun-formal cfun)]
                [cenv (closure-env c)])
           (eval-under-env
           (fun-body cfun)
           (extend-env (extend-env cenv cfname c) argsym arg)))
         (error "MUPL called something that wasnt a function")))]#| }}} |#
    [(mlet? e)#| {{{ |#
     (eval-under-env
       (mlet-body e)
       (extend-env env (mlet-var e) (eval-under-env (mlet-e e) env)))]#| }}} |#
    [(var? e)#| {{{ |#
     (envlookup env (var-string e))]#| }}} |#
    [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem C

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2) #| {{{ |#
  (letrec
    ([lethelper (lambda (letlist)
        (cond
          [(empty? letlist) e2]
          [else (mlet
                  (car (car letlist))
                  (cdr (car letlist))
                  (lethelper (cdr letlist)))]))])
    (lethelper lstlst)
    ))#| }}} |#

(define (ifeq e1 e2 e3 e4)
  (mlet*
    (list (cons "_x" e1) (cons "_y" e2))
    (ifgreater
      (var "_y")
      (add (int -1) (var "_x"))
      (ifgreater (var "_x") (add (int -1) (var "_y")) e3 e4) e4)))

;; Problem D

(define mupl-map
  (fun "mupl-map" "f"
       (fun #f "lst"
            (ifeq
              (isaunit (var "lst"))
              (int 1)
              (aunit)
              (apair (call (var "f") (fst (var "lst"))) (call (call (var "mupl-map") (var "f")) (snd (var "lst"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
    (fun #f "y" (call mupl-map (fun #f "x" (add (var "y") (var "x")))))))
