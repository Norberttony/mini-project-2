#lang plait

(require
  "../values.rkt"
  "../desugar.rkt"
  "../main.rkt")

(print-only-errors #true)

; Testing the = operator
(test (run `{= 32 43}) (numV 0))
(test (run `{= 23 23}) (numV 1))

; Testing the string value
(test (run `"I am string") (stringV "I am string"))
(test (run `{= "string1" "string1"}) (numV 1))

; Testing method matching
(test (run `{let1
             {o {object
                 {field a 1}
                 {method get-a {lam _ a}}}}
             {{o "get-a"} 0}}) (numV 1))
