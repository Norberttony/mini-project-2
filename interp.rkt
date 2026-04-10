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

(define (field? [m : Exp]) : Boolean
  (type-case Exp m
    [(fieldE _ __) #true]
    [else #false]))

(define (method? [m : Exp]) : Boolean
  (type-case Exp m
    [(methodE _ __) #true]
    [else #false]))

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
    [(objE members)
     (let* (
            [fields
             (map
              (lambda (m)
                (pair (fieldE-name m)
                      (interp (fieldE-val m) nv)))
              (filter field? members))]

            ; extend env with fields
            [field-env
             (foldl
              (lambda (p acc)
                (extend acc (fst p) (snd p)))
              nv
              fields)]

            ; build methods using extended env
            [methods
             (map
              (lambda (m)
                (pair (methodE-name m)
                      (interp (methodE-body m) field-env)))
              (filter method? members))])
       (objV fields methods))]

    ; send
    [(sendE obj name arg)
     (let ([ov (interp obj nv)]
           [av (interp arg nv)])
       (type-case Value ov
         [(objV fields methods)
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

    [(fieldE name body)
     (numV 0)]
    
    [(andE l r)
     (error 'interp "andE not desugared!")]
    [(orE l r)
     (error 'interp "orE not desugared!")]))
