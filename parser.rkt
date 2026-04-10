#lang plait

(require "ast.rkt")

; Parser helpers
(define (parse-field [s : S-Exp]) : Field
  (let ([l (s-exp->list s)])
    (field (s-exp->symbol (second l))
           (parse (third l)))))

(define (parse-method [s : S-Exp]) : Method
  (let* ([l (s-exp->list s)]
         [lam-part (s-exp->list (third l))])
    (method (s-exp->symbol (second l))
            (s-exp->symbol (second lam-part))
            (parse (third lam-part)))))

(define (field-exp? s)
  (and (s-exp-list? s)
       (symbol=? 'field (s-exp->symbol (first (s-exp->list s))))))

(define (method-exp? s)
  (and (s-exp-list? s)
       (symbol=? 'method (s-exp->symbol (first (s-exp->list s))))))

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

         ; Parse object
         [(and (s-exp-symbol? (first l))
               (symbol=? 'object (s-exp->symbol (first l))))
          (let ([parts (rest l)])
            (objectE
             (map parse-field (filter field-exp? parts))
             (map parse-method (filter method-exp? parts))))]
         
         ; Parse send
         [(and (s-exp-symbol? (first l))
               (symbol=? 'send (s-exp->symbol (first l))))
          (sendE (parse (second l))
                 (s-exp->symbol (third l))
                 (parse (fourth l)))]

         ; Parse class
        [(and (s-exp-symbol? (first l))
              (symbol=? 'class (s-exp->symbol (first l))))
         (classE
          (map s-exp->symbol (s-exp->list (second l)))
          (objectE
           (map parse-field (filter field-exp? (rest (rest l))))
           (map parse-method (filter method-exp? (rest (rest l))))))]
        
         [else
          (appE (parse (first l)) (parse (second l)))]))]
    [else
     (error 'parse "not a number, symbol, or list")]))

