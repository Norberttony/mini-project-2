#lang plait

(require
  "ast.rkt"
  "values.rkt")

; Handles binding of variable names to their values
; Also handles the concept of scope.

(define (lookup [name : Symbol] [env : Env]) : Value
  (cond
    [(empty? env)
     (error name "unbound variable")]
    [(symbol=? name (bind-name (first env)))
     (unbox (bind-val (first env)))]
    [else
     (lookup name (rest env))]))

(define (lookup-loc [name : Symbol] [env : Env]) : (Boxof Value)
  (cond
    [(empty? env)
     (error name "unbound variable")]
    [(symbol=? name (bind-name (first env)))
     (bind-val (first env))]
    [else
     (lookup-loc name (rest env))]))

(define (extend [env : Env] [name : Symbol] [val : Value]) : Env
  (cons (bind name (box val)) env))
