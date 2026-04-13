#lang plait

(require "ast.rkt")

; Type definitions for environments

(define-type Binding
  [bind (name : Symbol) (val : Value)])

(define-type-alias Env (Listof Binding))

(define empty-env : Env empty)

; Represents a runtime value.
(define-type Value
  [numV (n : Number)]
  [stringV (s : String)]
  [funV (var : Symbol) (body : Exp) (nv : Env)])
