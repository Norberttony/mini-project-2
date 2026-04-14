#lang plait

(require
  "ast.rkt"
  "env.rkt"
  "values.rkt")

(define (add [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'add "not a number")]))

(define (is-equal [l : Value] [r : Value]) : Value
  (if (equal? l r)
      (numV 1)
      (numV 0)))

(define (interp [e : Exp] [nv : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(stringE s) (stringV s)]
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]
    [(equalE l r) (is-equal (interp l nv) (interp r nv))]
    [(ifE cnd thn els)
     (if (equal? (interp cnd nv) (numV 0))
         (interp els nv)
         (interp thn nv))]
    [(lamE v b) (funV v b nv)]
    [(appE f a)
     (let ([fv (interp f nv)]
           [av (interp a nv)])
       (type-case Value fv
         [(funV v b nv*)
          (interp b (extend nv* v av))]
         [else
          (error 'app "not a function")]))]
    [(let1E var val body)
     (interp body (extend nv var (interp val nv)))]
    [(andE l r)
     (error 'interp "andE not desugared!")]
    [(orE l r)
     (error 'interp "orE not desugared!")]

    [(objE l r)
     (error 'interp "objE not desugared!")]
    [(fieldE l r)
     (error 'interp "fieldE not desugared!")]
    [(methodE l r)
     (error 'interp "methodE not desugared!")]
    [(classE params body)
     (error 'interp "classE not desugared!")]))
