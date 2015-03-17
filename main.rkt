#lang racket/base
(require "private/syntax.rkt"
         "private/ast.rkt")
(provide (all-from-out "private/syntax.rkt")
         ident?
         name?
         scalar-expr?
         table-ref?
         table-expr?
         statement?)
