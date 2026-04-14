#lang plait

(require "ast.rkt")

(define (desugar-fields [fields : (Listof Exp)] [body : Exp]) : Exp
  (foldr
   (lambda (f nested-fields)
     (type-case Exp f
       [(fieldE name val)
        (let1E name
               (desugar val)
               nested-fields)]
       [else
        (error 'desugar-fields "expected fieldE")]))
   body
   fields))

(define (desugar-methods [methods : (Listof Exp)])
  (foldr
   (lambda (m nested-methods)
     (type-case Exp m
       [(methodE name val)
        (ifE (equalE (varE 'msg) (stringE (symbol->string name)))
             (desugar val)
             nested-methods)]
       [else
        (error 'desugar-methods "expected methodE")]))
   (numE 0)
   methods))

(define (desugar [e : Exp]) : Exp
  (type-case Exp e
    [(numE n) (numE n)]
    [(stringE s) (stringE s)]
    [(varE s) (varE s)]
    [(plusE l r) (plusE (desugar l) (desugar r))]
    [(equalE l r) (equalE (desugar l) (desugar r))]
    [(ifE c t e) (ifE (desugar c) (desugar t) (desugar e))]
    [(lamE v b) (lamE v (desugar b))]
    [(appE f a) (appE (desugar f) (desugar a))]
    [(let1E v val b) (let1E v (desugar val) (desugar b))]
    [(andE l r)
     (desugar (ifE l r (numE 0)))]
    [(orE l r)
     (desugar
      (let1E 'v l
             (ifE (varE 'v)
                  (varE 'v)
                  r)))]

    ; desugaring object
    [(objE f m)
     (desugar-fields
      f
      (lamE 'msg
            (desugar-methods m)))]

    ; desugaring class
    [(classE params body)
     (foldr
      (lambda (p acc)
        (lamE p acc))
      (desugar body)
      params)]
    
    [(fieldE v b)
     (error 'desugar "fieldE desugaring not implemented!")]
    [(methodE v b)
     (error 'desugar "methodE desugaring not implemented!")]))
