#lang plait

(require
  "../values.rkt"
  "../main.rkt")

(print-only-errors #true)

;; numbers and addition
(test (run `3) (numV 3))
(test (run `{+ 3 4}) (numV 7))
(test (run `{+ {+ 1 2} {+ 3 4}}) (numV 10))

;; variables and let1
(test (run `{let1 {x 3} x}) (numV 3))
(test (run `{let1 {x 3} {+ x x}}) (numV 6))

;; shadowing
(test (run `{let1 {x 1}
                  {let1 {x 2} x}})
      (numV 2))

;; lambda immediate application
(test (run `{{lam x {+ x x}} 3}) (numV 6))

;; bind lambda to name
(test (run `{let1 {sq {lam x {+ x x}}}
                  {sq 5}})
      (numV 10))

;; lexical scope
(test (run `{let1 {x 3}
                  {let1 {f {lam y {+ x y}}}
                        {let1 {x 2}
                              {f 4}}}})
      (numV 7))

;; closure outside defining scope
(test (run `{{let1 {x 3} {lam y {+ x y}}} 4})
      (numV 7))

;; if core
(test (run `{if 1 42 99}) (numV 42))
(test (run `{if 0 42 99}) (numV 99))
(test (run `{if {+ 1 2} 7 8}) (numV 7))

;; and sugar
(test (run `{and 1 2}) (numV 2))
(test (run `{and 0 2}) (numV 0))
(test (run `{and 1 0}) (numV 0))

;; or sugar
(test (run `{or 3 99}) (numV 3))
(test (run `{or 0 99}) (numV 99))
(test (run `{or 0 0}) (numV 0))

;; or preserves value
(test (run `{let1 {x 5} {or x 99}}) (numV 5))

;; nested sugar
(test (run `{and {or 0 3} {+ 1 1}}) (numV 2))

;; sugar inside lambda
(test (run `{let1 {f {lam x {or x 10}}}
                  {f 0}})
      (numV 10))

;; errors
(test/exn (run `x) "unbound")
(test/exn (run `{f 3}) "unbound")

;; error: applying + to a function
(test/exn (run `{+ {{lam x x} {lam x x}} 1}) "not a number")
