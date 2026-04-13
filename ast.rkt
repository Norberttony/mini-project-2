#lang plait

; Provides a definition for Expressions.

(define-type Exp
  [numE (n : Number)]
  [plusE (left : Exp) (right : Exp)]
  [varE (name : Symbol)]
  [let1E (var : Symbol) (val : Exp) (body : Exp)]
  [lamE (var : Symbol) (body : Exp)]
  [appE (fun : Exp) (arg : Exp)]
  [ifE (cnd : Exp) (thn : Exp) (els : Exp)]
  [andE (left : Exp) (right : Exp)]
  [orE (left : Exp) (right : Exp)]
  [objE (fields : (Listof Exp)) (methods : (Listof Exp))]
  [fieldE (name : Symbol) (val : Exp)]
  [methodE (name : Symbol) (val : Exp)])
