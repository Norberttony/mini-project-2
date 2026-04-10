#lang plait

(require "ast.rkt")

;; helpers
(define (build-dispatch methods)
  (foldr
    (lambda (m acc)
      (ifE
        (appE (appE (varE 'equal?) (varE 'msg))
              (varE (method-name m)))
        (lamE (method-arg m)
              (desugar (method-body m)))
        acc))
    (numE 0)
    methods))

(define (wrap-fields fields body)
  (foldr
    (lambda (f acc)
      (let1E (field-name f)
             (desugar (field-value f))
             acc))
    body
    fields))


(define (desugar [e : Exp]) : Exp
  (type-case Exp e
    [(numE n) (numE n)]
    [(varE s) (varE s)]
    [(symE s) (symE s)]
    [(plusE l r) (plusE (desugar l) (desugar r))]
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

    ; Desugar objects
    [(objectE fields methods)
     (wrap-fields
      fields
      (lamE 'msg
            (build-dispatch methods)))]

    ; Desugar send
    [(sendE obj m arg)
     (appE
      (appE (desugar obj)
            (symE m))
      (desugar arg))]

    ; Desugar class
    [(classE params body)
     (foldr
      (lambda (p acc)
        (lamE p acc))
      (desugar body)
      params)]

    
    
    ))
