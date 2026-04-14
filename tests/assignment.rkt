#lang plait

(require
  "../values.rkt"
  "../desugar.rkt"
  "../main.rkt")

(print-only-errors #true)

; Example 1.1 showcasing a simple object with one method
(test (run `{let1 {obj {object
                        {method add1 {lam x {+ x 1}}}}}
                  {send obj "add1" 5}})
      (numV 6))

; Example 1.2 showcasing an object with many fields and many methods
(test (run `{let1 {obj {object
                        {field v1 1}
                        {field v2 2}
                        {method sum {lam _ {+ v1 v2}}}
                        {method sum-v2 {lam x {+ x v2}}}
                        {method is-v1 {lam x {= x v1}}}}}
                  {if {send obj "is-v1" 1}
                      {+ {send obj "sum" 0} {send obj "sum-v2" 3}}
                      {send obj "sum" 0}}})
      (numV 8))

; Example 2 showcasing mutable fields and state
(test (run `{let1 {o {object
                  {field count 0}
                  {method inc {lam _
                                   {set! count {+ count 1}}}}}}
              {let1 {v1 {send o "inc" 0}}
                    {send o "inc" 0}} })
      (numV 2))

; Example 3 showcasing two independent instances from a class.
; Since using set! and begin were outside of the scope of our project, we did not implement them.
(test (run `{let1 {constr {class {init}
                            {field val init}
                            {method get-val {lam _ init}}}}
                  {let1 {inst1 {constr 3}}
                        {let1 {inst2 {constr 5}}
                              {+ {send inst1 "get-val" 0} {send inst2 "get-val" 0}}}}})
      (numV 8))

; Example 4 showcasing that object fields are automatically private.
(test (run `{let1 {obj {object
                        {field secret 42}
                        {method reveal {lam _ secret}}}}
                  {send obj "reveal" 0}})
      (numV 42))

; Also from example 4, showing that there is an error if we access the field
(test/exn (run `{let1 {obj {object
                        {field secret 42}
                        {method reveal {lam _ secret}}}}
                  {+ secret 0}})
      "unbound")

; Example 5 showcasing dynamic dispatch
; Dynamic dispatch: where two classes respond differently to the same message
(test (run `{let1 {make-adder {class {n}
                                {method compute {lam x {+ x n}}}}}
                  {let1 {make-doubler {class {n}
                                        {method compute {lam x {+ x x}}}}}
                        {let1 {a {make-adder 10}}
                              {let1 {d {make-doubler 0}}
                                    {+ {send a "compute" 5}
                                       {send d "compute" 5}}}}}})
      (numV 25))
