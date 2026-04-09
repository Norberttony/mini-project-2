#lang plait

(require "ast.rkt")

; Converts S-Exp -> Exp

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-number? s)
     (numE (s-exp->number s))]
    [(s-exp-symbol? s)
     (varE (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(and (s-exp-symbol? (first l))
               (symbol=? '+ (s-exp->symbol (first l))))
          (plusE (parse (second l)) (parse (third l)))]
         [(and (s-exp-symbol? (first l))
               (symbol=? 'if (s-exp->symbol (first l))))
          (ifE (parse (second l))
               (parse (third l))
               (parse (fourth l)))]
         [(and (s-exp-symbol? (first l))
               (symbol=? 'and (s-exp->symbol (first l))))
          (andE (parse (second l)) (parse (third l)))]
         [(and (s-exp-symbol? (first l))
               (symbol=? 'or (s-exp->symbol (first l))))
          (orE (parse (second l)) (parse (third l)))]
         [(and (s-exp-symbol? (first l))
               (symbol=? 'lam (s-exp->symbol (first l))))
          (lamE (s-exp->symbol (second l))
                (parse (third l)))]
         [(and (s-exp-symbol? (first l))
               (symbol=? 'let1 (s-exp->symbol (first l))))
          (let ([binding (s-exp->list (second l))])
            (let1E (s-exp->symbol (first binding))
                   (parse (second binding))
                   (parse (third l))))]
         [else
          (appE (parse (first l)) (parse (second l)))]))]
    [else
     (error 'parse "not a number, symbol, or list")]))

