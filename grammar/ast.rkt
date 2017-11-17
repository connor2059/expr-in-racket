#lang racket/base
;(require racket/generic)
(provide (all-defined-out))

;parser
(define-struct constant (value)#:prefab);
(define-struct definition (id value)#:prefab);
(define-struct box-definition (id value)#:prefab);
(define-struct box-assign (id value)#:prefab);
(define-struct unbox-expr(value)#:prefab)
(define-struct assign (id value)#:prefab);
(define-struct sub-assign(expr sublist value)#:prefab);
(define-struct func-definition(id formlist value)#:prefab)
(define-struct block-expr (block value)#:prefab)
(define-struct binding-block-expr (bindings block value)#:prefab)
(define-struct binding (id value)#:prefab)
(define-struct pair-expr (car cdr)#:prefab)
(define-struct pair-exprlist (lst else)#:prefab)
(define-struct variable (sym)#:prefab);
(define-struct seq (first second end)#:prefab)
(define-struct array-expr(lst)#:prefab)
(define-struct hash-expr (name index nfd)#:prefab)
;(define-struct seq-inf (first second)#:prefab)
;(define-struct assignment (expr value)#:prefab);var ass
;(define-struct free-assignment(expr value)#:prefab);var
(define-struct operation (sym parameters)#:prefab);
(define-struct call (expr sublist)#:prefab);
(define-struct opt-call(id formlist)#:prefab)
(define-struct when-statement (test block)#:prefab)
(define-struct while-statement (test block)#:prefab)
(define-struct do-statement (test block)#:prefab)
(define-struct if-statement (test block else-block)#:prefab)
(define-struct cond-statement (test-blocks else-block)#:prefab)
(define-struct if-expr (test block else-block)#:prefab)
(define-struct for-statement(bindings test update block)#:prefab)
(define-struct sub-extract (expr sublist)#:prefab);

;----------------------------------------------------
;(define-struct if-statement (ifcond body)#:prefab)
;(define-struct ifelse-statement (ifcond body elsebody)#:prefab)