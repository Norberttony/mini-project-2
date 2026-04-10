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

(define (interp [e : Exp] [nv : Env]) : Value
  (type-case Exp e
    [(numE n) (numV n)]
    [(varE s) (lookup s nv)]
    [(plusE l r) (add (interp l nv) (interp r nv))]

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

    ; creating objects
    [(objE methods)
     (objV
      (map
       (lambda (m)
         (type-case Exp m
           [(methodE name body)
            (pair name (interp body nv))]
           [else (error 'obj "not a method")]))
       methods))]

    ; send
    [(sendE obj name arg)
     (let ([ov (interp obj nv)]
           [av (interp arg nv)])
       (type-case Value ov
         [(objV methods)
          (let ([mv (lookup-method name methods)])
            (type-case Value mv
              [(funV v b nv*)
               (interp b (extend nv* v av))]
              [else
               (error 'send "method not a function")]))]
         [else
          (error 'send "not an object")]))]

    ; method
    [(methodE name body)
     (methodV name (interp body nv))]
    
    [(andE l r)
     (error 'interp "andE not desugared!")]
    [(orE l r)
     (error 'interp "orE not desugared!")]))
