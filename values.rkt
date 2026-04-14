#lang plait

(require "ast.rkt")

; Represents a runtime value.
(define-type Value
  [numV (n : Number)]
  [stringV (s : String)]
  [funV (var : Symbol) (body : Exp) (nv : Env)])


; Type definitions for environments

(define-type Binding
  [bind (name : Symbol) (val : (Boxof Value))])

(define-type-alias Env (Listof Binding))

(define empty-env : Env empty)

