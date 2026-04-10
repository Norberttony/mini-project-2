#lang plait

; Provides a definition for Expressions.

(define-type Field
  [field (name : Symbol) (value : Exp)])

(define-type Method
  [method (name : Symbol) (arg : Symbol) (body : Exp)])

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
  [objectE (fields : (Listof Field)) (methods : (Listof Method))]
  [sendE (obj : Exp) (method : Symbol) (arg : Exp)]
  [classE (params : (Listof Symbol)) (body : Exp)]
  [symE (s : Symbol)])
