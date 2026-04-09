#lang plait

(require "ast.rkt")

(define (desugar [e : Exp]) : Exp
  (type-case Exp e
    [(numE n) (numE n)]
    [(varE s) (varE s)]
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
                  r)))]))
