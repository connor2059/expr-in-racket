#lang racket/base
(require  (for-syntax "compiler.rkt")
          (for-syntax racket/base)
         )

(define-syntax (expr: syntax-object)
  (syntax-case syntax-object ()
    ((_ expr-string)
     (with-syntax([x (datum->syntax syntax-object (syntax->datum(expr-expander(format "~a" (syntax->datum #'expr-string)))))])
       #'x))))

(provide expr:)

(module+ test
(expr: "(x->1,y->2)=>x+y")
(expr: "(x->3,y->2){z:=x*y;}=>x+y-z")
(define(f x)
    (expr: "sin(x)+1" ))
(expr: "{g(x):={z:=8;z=z+1;}=>z*x;}=>g(6)")
(f 6)
)
