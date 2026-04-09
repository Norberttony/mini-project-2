#lang plait

(require
  "values.rkt"
  "parser.rkt"
  "interp.rkt"
  "desugar.rkt")

; Primary entry point

(define (run [s : S-Exp]) : Value
  (interp (desugar (parse s)) empty-env))
