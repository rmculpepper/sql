#lang racket/base
(require racket/contract/base
         "private/syntax.rkt"
         "private/ast.rkt"
         "private/emit.rkt"
         "private/dynamic.rkt")

(provide ;; from private/ast.rkt
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
   (-> any/c scalar-expr-ast?)]))
