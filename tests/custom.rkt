#lang plait

(require
  "../values.rkt"
  "../desugar.rkt"
  "../main.rkt")

(print-only-errors #true)

; Testing the = operator
(test (run `{= 32 43}) (numV 0))
(test (run `{= 23 23}) (numV 1))
