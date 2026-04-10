#lang plait

(require
  "../main.rkt"
  "../values.rkt")

; Allow any number of methods.
(test
 (run `{let1 {obj {object
                   {method add1 {lam x {+ x 1}}}
                   {method add2 {lam x {+ x 2}}}}}
             {send obj "add2" 5}}
      ) (numV 7))

; Allow fields
(test
 (run `{let1 {obj {object
                   {field myVal 3}
                   {method get1 {lam _ myVal}}}}
             {send obj "get1"}}
      ) (numV 3))
