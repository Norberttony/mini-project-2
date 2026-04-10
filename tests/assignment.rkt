#lang plait

(require
  "../main.rkt"
  "../values.rkt")

(test
 (run `{let1 {obj {object
                   {method add1 {lam x {+ x 1}}}}}
             {send obj "add1" 5}}
      ) (numV 6))
