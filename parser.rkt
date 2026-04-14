#lang plait

(require "ast.rkt")

; Converts S-Exp -> Exp

(define (field-exp? s)
  (and (s-exp-list? s)
       (symbol=? 'field (s-exp->symbol (first (s-exp->list s))))))

(define (method-exp? s)
  (and (s-exp-list? s)
       (symbol=? 'method (s-exp->symbol (first (s-exp->list s))))))

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-number? s)
     (numE (s-exp->number s))]
    [(s-exp-string? s)
     (stringE (s-exp->string s))]
    [(s-exp-symbol? s)
     (varE (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([l (s-exp->list s)])
       (cond
         [(and (s-exp-symbol? (first l))
               (symbol=? '+ (s-exp->symbol (first l))))
          (plusE (parse (second l)) (parse (third l)))]
         [(and (s-exp-symbol? (first l))
               (symbol=? '= (s-exp->symbol (first l))))
          (equalE (parse (second l)) (parse (third l)))]
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

         ; object
         [(and (s-exp-symbol? (first l))
               (symbol=? 'object (s-exp->symbol (first l))))
          (let ([parts (rest l)])
            (objE
             (map parse (filter field-exp? parts))
             (map parse (filter method-exp? parts))))]

         ; field
         [(and (s-exp-symbol? (first l))
               (symbol=? 'field (s-exp->symbol (first l)))
               (s-exp-symbol? (second l)))
          (fieldE (s-exp->symbol (second l)) (parse (third l)))]

         ; method
         [(and (s-exp-symbol? (first l))
               (symbol=? 'method (s-exp->symbol (first l)))
               (s-exp-symbol? (second l)))
          (methodE (s-exp->symbol (second l)) (parse (third l)))]

         ; send
         [(and (s-exp-symbol? (first l))
               (symbol=? 'send (s-exp->symbol (first l))))
          (appE
           (appE (parse (second l)) (parse (third l)))
           (parse (fourth l)))]
         
         [else
          (appE (parse (first l)) (parse (second l)))]))]
    [else
     (error 'parse "not a number, symbol, or list")]))

