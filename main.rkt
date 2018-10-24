#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         "private/syntax.rkt"
         "private/ast.rkt"
         "private/emit.rkt"
         "private/dynamic.rkt")

(provide
 ;; from private/ast.rkt
 statement-ast?
 ddl-ast?
 table-ref-ast?
 table-expr-ast?
 scalar-expr-ast?
 ident-ast?
 name-ast?

 ;; from private/syntax.rkt
 name-qq
 ident-qq
 table-ref-qq
 table-expr-qq
 scalar-expr-qq
 statement-qq
 select-qq
 ddl-qq

 sql
 select
 insert
 update
 delete
 create-table
 create-view

 sql-statement?)

(provide
 (contract-out
  [sql-ast->string
   (->* [(or/c name-ast? scalar-expr-ast? table-ref-ast? table-expr-ast? statement-ast? ddl-ast?)]
        [(or/c symbol? #f)]
        string?)]
  [sql-statement->string
   (->* [sql-statement?] [(or/c symbol? #f)] string?)]
  [current-sql-dialect
   (parameter/c (or/c symbol? #f))]

  [make-ident-ast
   (-> (or/c symbol? string?) ident-ast?)]
  [make-name-ast
   (-> (flat-rec-contract C symbol? name-ast? (listof C)) name-ast?)]
  [value->scalar-expr-ast
   (-> any/c scalar-expr-ast?)]
  [make-values*-table-expr-ast
   (-> (and/c (listof (listof scalar-expr-ast?))
              ;; rows-same-length/c must come second
              rows-same-length/c)
       table-expr-ast?)]))

;; rows-same-length/c:
;;   contract version of check-same-length from "private/ast.rkt".
;; Assumes that value to be checked would satisfy (listof (listof any/c)),
;;   so use this as the second case in an and/c contract.
(define rows-same-length/c
  (flat-contract-with-explanation
   #:name 'rows-same-length/c
   (λ (l-rows)
     (or (check-same-length l-rows)
         (λ (blame)
           (raise-blame-error
            blame l-rows
            '("all rows must be the same length"
              given: "~e"
              "\n  " given " lengths: ~e")
            l-rows
            (map length l-rows)))))))


