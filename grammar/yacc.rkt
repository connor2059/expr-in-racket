#lang racket/base
(require "lex.rkt"
         "ast.rkt"
         "../srcloc.rkt"
         parser-tools/lex
         parser-tools/yacc
         )

(provide parse-expr)

(define-struct (exn:fail:parser exn:fail) (lang tok-ok? tok-name tok-value pos)#:transparent)
(define(position->srcloc p)
  (make-srcloc (file-path)
               (position-line p)
               (position-col p)
               (position-offset p)
               #f))
;语法
(define  expr-parser
 (parser
   (start  expr)
   (end  EOF)
   (tokens value-tokens op-tokens)
   (error
     (lambda (tok-ok? tok-name tok-value . x )
       ;(error  (list tok-ok? tok-name tok-value  x))
       (raise (make-exn:fail:read "expr:error"(current-continuation-marks) (map position->srcloc x)  ))))

    (src-pos)
   ;(debug  "D:\\debug.txt")
  ; (yacc-output "D:\\out.txt")
   (precs 
          ;(left ?)
          (nonassoc =>)
          (right :=)
          (left ~ TILDE)
          (nonassoc ?)
          (nonassoc :)
          (left or xor and !)
          (left + -)
          (left * / //) 
          (nonassoc > < >= <= = !=)
          (left UMINUS UPLUS)
          (right ^)
          (left  @)
          ;(nonassoc ... in)
          (nonassoc mod);先算
          (left $)
          (left COMMA)
          (nonassoc OP MOP BOP)
         
          )
   (grammar 
   ; (prog [(statements) $1])
   (statements
           ; [()'()]
            [(statement)(list $1)]
            [(statements  statement)  `(,@$1 ,$2)]
           )
   (statement[(definition )$1]
             [(assignment )$1]
             ;[(optcall)$1]
             [(when-statement)$1]
             ;[(if-statement)$1]
             ;[(cond-statement)$1]
             ;[(while-statement)$1]
             ;[(do-statement)$1]
             ;[(for-statement)$1]
             )
   (definition[(SYMBOL := expr |;|)(make-definition $1 $3)]
              [(SYMBOL OP formallist CP := expr |;|)(make-func-definition $1 $3 $6)]
              [(& SYMBOL := expr |;|)(make-box-definition $2 $4)]);-------------
   (assignment[(pure-assignment)$1]
              [(box-assignment)$1]
              )
   (pure-assignment[(SYMBOL = expr |;|)(make-assign $1 $3)]);---------------------
   (box-assignment[(& SYMBOL = expr |;|)(make-box-assign $2 $4)])
   ;(optcall[(expr @ MOP sublist MCP |;|)(make-opt-call $1 $4)])
   (when-statement [(WHEN OP expr CP BOP statements BCP)(make-when-statement $3 $6)])
   ;(if-statement [(IF OP expr CP BOP statements BCP BOP statements BCP)(make-if-statement $3 $6 $9)])
   ;(cond-statement[(COND text-blocks ELSE  BOP statements BCP)(make-cond-statement $2 $5)])
   ;(while-statement[(WHILE OP expr CP BOP statements BCP)(make-while-statement $3 $6)])
   ;(do-statement[(DO BOP statements BCP OP expr CP)(make-do-statement  $6 $3)])
   ;(for-statement[(FOR OP binds |:| expr |:| statements CP BOP statements BCP)(make-for-statement  $3 $5 $7 $10)])
   ;(text-blocks [(text-block)(list $1)]
   ;             [(text-blocks text-block)`(,@$1 ,$2)])
   ;(text-block [(expr : BOP statements BCP)(cons $1 $4)])
                
    (expr [(SYMBOL)(make-variable $1)]
          [(NUM_CONST)(make-constant $1)]
          [(STR_CONST)(make-constant $1)]
          [(BOOLEAN)(make-constant $1)]
          ;----------------------
          [($ expr)(make-unbox-expr $2)]
          [(- expr) (prec UMINUS) (make-operation '- (list $2))]
          [(+ expr) (prec UMINUS) (make-operation '- (list $2))]

          ;------------------------------------------
        ;  [(expr :  expr option expr)(make-operation 'inrange (list $1 $3 $5))]
        ;  [(expr :  expr)(make-operation 'inrange (list $1 $3))]
          [(expr +  expr)(make-operation '+ (list $1 $3))]
          [(expr -  expr)(make-operation '- (list $1 $3))]
          [(expr *  expr)(make-operation '* (list $1 $3))]
          [(expr /  expr)(make-operation '/ (list $1 $3))]
          [(expr //  expr)(make-operation 'quotient(list $1 $3))]
          [(expr ^  expr)(make-operation 'expt (list $1 $3))]
          [(expr mod  expr)(make-operation 'modulo (list $1 $3))]
          [(OP expr CP)$2]
        ;  [(expr ~  expr)(make-operation '~ (list $1 $3))]
          ;[(expr ?  expr)(make-operation '? (list $1 $3))]
          [(expr >  expr)(make-operation '> (list $1 $3))]
          [(expr <  expr)(make-operation '<(list $1 $3))]
          [(expr >=  expr)(make-operation '>= (list $1 $3))]
          [(expr <=  expr)(make-operation '<= (list $1 $3))]
        ;  [(expr =  expr)(make-operation 'equal? (list $1 $3))]
          [(expr and  expr)(make-operation 'and (list $1 $3))]
          [(expr or  expr)(make-operation 'or (list $1 $3))]
          [(expr xor  expr)(make-operation 'xor (list $1 $3))]
          [(expr @ expr : expr)(make-hash-expr   $1 $3 $5)]
          [(BOP statements BCP => expr)(make-block-expr $2 $5)]
          [(OP binds CP BOP statements BCP => expr)(make-binding-block-expr $2 $5 $8)]
          [(OP binds CP  => expr)(make-binding-block-expr $2 '() $5)]
        ;  [(expr in slice)(make-operation 'in (list $1 $3))]
        ;  [(expr is expr)(make-operation 'is (list $1 $3))]
          [(expr OP sublist CP)(make-call $1 $3)]
          [(expr ?  expr : expr)(make-if-expr $1 $3 $5)]
          ;  [(sub-expr)$1]
          [(expr MOP sublist MCP)(make-sub-extract $1 $3)]
          ;---------------------------------------------------------

          ;---------------------------------------------------------
          ;-------------------------------------------------------
           [[MOP sublist MCP](make-array-expr $2)]
          ; (for-expr[(FOR OP binds |:| expr |:| statements CP BOP statements => expr |;| BCP)(make-for-expr  $3 $5 $7 $10 $13)])
      ;    [(BOP pair-exprlist |;| expr BCP) (make-pair-exprlist $2 $4)]

          ;----------------
          )
    ;  (sub-expr[(expr MOP sublist MCP)(make-sub-extract $1 $3)])
     (sublist [(expr)(list $1)]
             [(sublist COMMA expr)(append $1 (list $3))])
     (formallist[(SYMBOL)(list $1)]
                [(formallist COMMA SYMBOL)(append $1 (list $3))])
     (binds [(bind)(list $1)]
            [(binds COMMA bind) `(,@$1 ,$3)])
     (bind [(SYMBOL -> expr) (cons $1 $3)])
    #|
    (slice[(expr COMMA  expr COMMA ... COMMA expr)(make-seq $1 $3 $7)]
          [(expr COMMA  expr COMMA ...)(make-seq $1 $3 #f)])
    (pair-exprlist[(pair-expr)(list $1)]
                  [(pair-exprlist |;| pair-expr) (append $1 (list $3))])
    (pair-expr [(expr COMMA expr)(make-pair-expr $1 $3)])
    
   
|#
    ;
      )
 ; (suppress)
   ))

; 
;(define (list program-parser statements-parser definitionmet-parser expr-parser) r-parser)

(define ((mk-parser which) ip)
  (define (go)
    (port-count-lines! ip)
    (which (lambda () (rlexer ip))))
  (if (current-source-name)
      (go)
      (parameterize ([current-source-name (object-name ip)]
                     [file-path (object-name ip)])
        (go))))
;(define parse-statement (mk-parser program-parser))
(define parse-expr (mk-parser expr-parser))
;(define p parse-program)
;(parse-statement  (open-input-string "f(x , y)"))
;(parse-statement  (open-input-string "f(8 8)"))
(module+ test
;(parse-expr  (open-input-string "(a->1,b->2){&x:=[1,2,a,b];&x=$x+1+b;if(x>0){x=x-1;}{x=x+1;}}=>(a->1,b->2){&x:=b;&x=$x+1+b;}=>x>1 ? 1 : a+b-$x;"))
;(parse-program  (open-input-string "&x[1]=3 4"))
(parse-expr  (open-input-string "a+b"))
)