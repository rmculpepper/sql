;; Parsing

#lang racket/base
(require syntax/parse
         (only-in syntax/parse [attribute $])
         (rename-in racket/match [match-define defmatch])
         "ast.rkt")
(provide (all-defined-out))

;; ============================================================
;; Entry points

(define (parse-select stx)
  (syntax-parse stx [x:Select ($ x.ast)]))
(define (parse-table-ref stx)
  (syntax-parse stx [x:TableRef ($ x.ast)]))
(define (parse-table-expr stx)
  (syntax-parse stx [x:TableExpr ($ x.ast)]))
(define (parse-scalar-expr stx)
  (syntax-parse stx [x:ScalarExpr ($ x.ast)]))


;; ============================================================
;; Select Statements

(define-syntax-class Select
  #:attributes (ast)
  #:datum-literals (select)
  (pattern (~and (select . _) :SelectInner)))

(define-syntax-class SelectInner
  #:attributes (ast)
  (pattern (_ vs:SelectValues
              (~or (~optional sel:SelectValuesClause)
                   (~optional from:SelectFromClause)
                   (~optional where:SelectWhereClause)
                   (~optional groupby:SelectGroupByClause)
                   (~optional having:SelectHavingClause)
                   (~optional order:SelectOrderClause)
                   (~optional limit:SelectLimitClause)
                   (~optional offset:SelectOffsetClause))
              ...)
           #:fail-when (and (pair? ($ vs.ast)) ($ sel.kw))
                       "#:values clause not allowed with initial value list"
           #:fail-when (and (pair? ($ having.ast))
                            (not (pair? ($ groupby.columns))))
                       "#:having clause with empty #:group-by"
           #:attr ast (stmt:select (append ($ vs.ast) (or ($ sel.ast) null))
                                   (or ($ from.ast) null)
                                   (or ($ where.ast) null)
                                   (or ($ groupby.columns) null)
                                   (or ($ having.ast) null)
                                   (and (or ($ order.ast) ($ limit.ast) ($ offset.ast))
                                        (select:extension
                                         (or ($ order.ast) null)
                                         ($ limit.ast)
                                         ($ offset.ast))))))

(define-splicing-syntax-class SelectValues
  #:attributes ([ast 1])
  (pattern (~seq :SelectItem ...)))

(define-splicing-syntax-class SelectValuesClause
  #:attributes ([ast 1] kw)
  (pattern (~seq (~and #:values kw) :SelectValues)))

(define-syntax-class SelectItem
  #:attributes (ast)
  #:datum-literals (as)
  (pattern (as expr:ScalarExpr column:Ident)
           #:attr ast (select-item:as ($ expr.ast) ($ column.sym)))
  (pattern expr:ScalarExpr
           #:attr ast ($ expr.ast)))

(define-splicing-syntax-class SelectFromClause
  #:attributes ([ast 1])
  (pattern (~seq #:from :TableRef ...)))

(define-splicing-syntax-class SelectWhereClause
  #:attributes ([ast 1])
  (pattern (~seq #:where :ScalarExpr ...)))

(define-splicing-syntax-class SelectGroupByClause
  #:attributes (columns)
  (pattern (~seq #:group-by c:Ident ...)
           #:attr columns ($ c.sym)))

(define-splicing-syntax-class SelectHavingClause
  #:attributes ([ast 1])
  (pattern (~seq #:having :ScalarExpr ...)))

(define-splicing-syntax-class SelectOrderClause
  #:attributes ([ast 1])
  (pattern (~seq #:order-by :SelectOrderItem ...)))
(define-splicing-syntax-class SelectOrderItem
  #:attributes (ast)
  (pattern (~seq e:ScalarExpr o:SelectOrderDirection)
           #:attr ast (select:order ($ e.ast) ($ o.dir))))
(define-splicing-syntax-class SelectOrderDirection
  #:attributes (dir)
  (pattern (~seq #:asc) #:attr dir 'asc)
  (pattern (~seq #:desc) #:attr dir 'desc)
  (pattern (~seq) #:attr dir #f))

(define-splicing-syntax-class SelectLimitClause
  #:attributes (ast)
  (pattern (~seq #:limit :ScalarExpr)))

(define-splicing-syntax-class SelectOffsetClause
  #:attributes (ast)
  (pattern (~seq #:offset :ScalarExpr)))


;; ============================================================
;; Table References && Expressions

(define-syntax-class TableRef
  #:attributes (ast)
  #:datum-literals (as)
  (pattern table-name:Ident
           #:attr ast (table-ref:id ($ table-name.sym)))
  (pattern (as table-name:Ident range-var:Ident)
           #:attr ast (table-ref:as (table-ref:id ($ table-name.sym))
                                    ($ range-var.sym)))
  (pattern (as t:TableExpr range-var:Ident)
           #:attr ast (table-ref:as ($ t.ast) ($ range-var.sym)))
  (pattern :TableExpr))

(define-syntax-class TableExpr
  #:attributes (ast)
  #:datum-literals (cross-join values values*)
  (pattern (cross-join t1:TableRef t2:TableRef)
           #:attr ast (table-expr:cross-join ($ t1.ast) ($ t2.ast)))
  (pattern (j:Join t1:TableRef t2:TableRef :join-on-clause)
           #:attr ast (table-expr:join (syntax-e #'j) ($ t1.ast) ($ t2.ast) ($ on)))
  (pattern (so:SetOp t1:expr t2:expr :maybe-all :set-op-clause)
           #:attr ast (table-expr:set-op (syntax-e #'so)
                                         (parse-table-expr #'t1)
                                         (parse-table-expr #'t2)
                                         (attribute all?)
                                         (attribute corr)))
  (pattern (values e:expr ...)
           #:attr ast (table-expr:values
                       (list (map parse-scalar-expr (syntax->list #'(e ...))))))
  (pattern (values* [e:expr ...] ...)
           #:attr ast (table-expr:values
                       (for/list ([es (syntax->list #'((e ...) ...))])
                         (for/list ([e (syntax->list es)])
                           (parse-scalar-expr e)))))
  (pattern s:Select
           #:attr ast (table-expr:select ($ s.ast))))

(define-syntax-class Join
  (pattern (~datum inner-join))
  (pattern (~datum left-join))
  (pattern (~datum right-join))
  (pattern (~datum full-join)))
(define-syntax-class SetOp
  (pattern (~datum union))
  (pattern (~datum intersect))
  (pattern (~datum except)))

(define-splicing-syntax-class set-op-clause
  (pattern (~seq #:corresponding)
           #:attr corr 'auto)
  (pattern (~seq #:corresponding-by (column:id ...))
           #:attr corr (syntax->datum #'(column ...)))
  (pattern (~seq)
           #:attr corr #f))

(define-splicing-syntax-class maybe-all
  (pattern (~seq #:all) #:attr all? #t)
  (pattern (~seq #:all) #:attr all? #f))

(define-splicing-syntax-class join-on-clause
  (pattern (~seq #:natural)
           #:attr on '(natural))
  (pattern (~seq #:using (column:id ...))
           #:attr on `(using ,(syntax->datum #'(column ...))))
  (pattern (~seq #:on condition:ScalarExpr)
           #:attr on `(on ,($ condition.ast))))

;; ============================================================
;; Scalar Expressions

(define-syntax-class ScalarExpr
  #:attributes (ast)
  #:datum-literals (literal ?)
  (pattern n:exact-integer
           #:attr ast (syntax-e #'n))
  (pattern s:str
           #:attr ast (syntax-e #'s))
  (pattern var:Ident
           #:attr ast ($ var.sym))
  (pattern ?
           #:attr ast (scalar:placeholder))
  (pattern (literal s:str)
           #:attr ast (scalar:literal (syntax-e #'s)))
  (pattern (o:Op arg:ScalarExpr ...)
           #:fail-unless (arity-includes? (op-arity (attribute o.op))
                                          (length (syntax->list #'(arg ...))))
                         "wrong arity"
           #:attr ast (scalar:app (attribute o.op) (attribute arg.ast))))

(define-syntax-class Op
  #:attributes (op)
  (pattern o:Ident
           #:attr op (cond [(assq ($ o.sym) standard-ops) => cadr] [else #f])
           #:when ($ op)))

;; ============================================================
;; Other

(define-syntax-class Ident
  #:attributes (sym)
  (pattern (~and x:id (~not (~datum ?)))
           #:attr sym (syntax-e #'x)))
