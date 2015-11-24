;; Parsing

#lang racket/base
(require syntax/parse
         (only-in syntax/parse [attribute $])
         (rename-in racket/match [match-define defmatch])
         "ast.rkt"
         ;; For unquote-related contracts:
         (for-template racket/base "ast.rkt"))
(provide (all-defined-out))

;; ============================================================
;; Entry points

(define (parse-statement stx)
  (syntax-parse stx
    [x:Select ($ x.ast)]
    [x:Insert ($ x.ast)]
    [x:Update ($ x.ast)]
    [x:Delete ($ x.ast)]))

(define (parse-table-ref stx)
  (syntax-parse stx [x:TableRef ($ x.ast)]))
(define (parse-table-expr stx)
  (syntax-parse stx [x:TableExpr ($ x.ast)]))
(define (parse-scalar-expr stx)
  (syntax-parse stx [x:ScalarExpr ($ x.ast)]))

;; ============================================================
;; Statements

;; The following stxclasses recognize the statement type by symbol. In
;; contrast, macro versions will use the Inner stxclasses directly.

(define-syntax-class Statement
  #:attributes (ast)
  (pattern :With)
  (pattern :Select)
  (pattern :Insert)
  (pattern :Update)
  (pattern :Delete))

(define-syntax-class With
  #:attributes (ast)
  (pattern (~and ((~datum with) . _) :WithInner)))
(define-syntax-class Select
  #:attributes (ast)
  (pattern (~and ((~datum select) . _) :SelectInner)))
(define-syntax-class Insert
  #:attributes (ast)
  (pattern (~and ((~datum insert) . _) :InsertInner)))
(define-syntax-class Update
  #:attributes (ast)
  (pattern (~and ((~datum update) . _) :UpdateInner)))
(define-syntax-class Delete
  #:attributes (ast)
  (pattern (~and ((~datum delete) . _) :DeleteInner)))

;; ============================================================
;; With Statement

(define-syntax-class WithInner
  #:description #f
  #:attributes (ast)
  (pattern (_ rec:MaybeRec ([name:Ident rhs:SelectOrWith] ...) body:Statement)
           #:attr ast (statement:with ($ rec.ast) ($ name.ast) ($ rhs.ast) ($ body.ast))))

(define-splicing-syntax-class MaybeRec
  #:description #f
  #:attributes (ast)
  (pattern (~seq #:recursive) #:attr ast #t)
  (pattern (~seq) #:attr ast #f))

(define-syntax-class SelectOrWith
  #:attributes (ast)
  (pattern :Select)
  (pattern (_ rec:MaybeRec ([name:Ident rhs:Select] ...) body:SelectOrWith)
           #:attr ast (statement:with ($ rec.ast) ($ name.ast) ($ rhs.ast) ($ body.ast))))

;; ============================================================
;; Select Statement

(define-syntax-class SelectInner
  #:description #f
  #:attributes (ast)
  (pattern (_ vs:SelectValues
              (~or (~optional sel:SelectValuesClause)
                   (~optional from:SelectFromClause)
                   (~optional where:WhereClause)
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
           #:attr ast (statement:select (append ($ vs.ast) (or ($ sel.ast) null))
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
  #:datum-literals (as *)
  (pattern (as ~! expr:ScalarExpr column:Ident)
           #:attr ast (select-item:as ($ expr.ast) ($ column.ast)))
  (pattern *
           ;; FIXME: add qualified.* support
           #:attr ast (select-item:all))
  (pattern expr:ScalarExpr
           #:attr ast ($ expr.ast)))

(define-splicing-syntax-class SelectFromClause
  #:attributes ([ast 1])
  (pattern (~seq #:from :TableRef ...)))

(define-splicing-syntax-class WhereClause
  #:attributes ([ast 1])
  (pattern (~seq #:where :ScalarExpr ...)))

(define-splicing-syntax-class SelectGroupByClause
  #:attributes (columns)
  (pattern (~seq #:group-by c:Name ...)
           #:attr columns ($ c.ast)))

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
;; Insert Statement

;; TODO: want to also support following syntax:
;;   (insert table ([column expr] ...))

;; (insert #:into table #:set [column expr] ...)
;; (insert #:into table #:columns column ... #:values expr ...)
;; (insert #:into table #:columns column ... #:from TableExpr)

(define-syntax-class InsertInner
  #:description #f
  #:attributes (ast)
  (pattern (_ target:InsertTarget
              assign:AssignClause)
           #:attr ast (statement:insert
                       ($ target.ast)
                       (map update:assign-column ($ assign.ast))
                       (table-expr:values
                        (list (map update:assign-expr ($ assign.ast))))))
  (pattern (_ target:InsertTarget
              (~optional cols:InsertColumns)
              src:InsertSource)
           #:attr ast (statement:insert ($ target.ast) ($ cols.ast) ($ src.ast))))

(define-splicing-syntax-class InsertTarget
  #:attributes (ast)
  (pattern (~seq #:into :Name)))

(define-splicing-syntax-class InsertColumns
  #:attributes ([ast 1])
  (pattern (~seq #:columns :Ident ...)))

(define-splicing-syntax-class InsertSource
  #:attributes (ast)
  (pattern (~seq #:values e:ScalarExpr ...)
           #:attr ast (table-expr:values (list ($ e.ast))))
  (pattern (~seq #:from :TableExpr)))

;; ============================================================
;; Update Statement

(define-syntax-class UpdateInner
  #:description #f
  #:attributes (ast)
  (pattern (_ table:Name
              (~or (~once assign:AssignClause)
                   (~optional where:WhereClause))
              ...)
           #:attr ast (statement:update ($ table.ast) ($ assign.ast)
                                   (or ($ where.ast) null))))

(define-splicing-syntax-class AssignClause
  #:attributes ([ast 1])
  (pattern (~seq #:set :Assignment ...)))

(define-syntax-class Assignment
  #:attributes (ast)
  (pattern [c:Ident e:ScalarExpr]
           #:attr ast (update:assign ($ c.ast) ($ e.ast))))

;; ============================================================
;; Delete Statement

(define-syntax-class DeleteInner
  #:description #f
  #:attributes (ast)
  (pattern (_ (~or (~once :DeleteFromClause)
                   (~optional where:WhereClause))
              ...)
           #:attr ast (statement:delete ($ table) (or ($ where.ast) null))))

(define-splicing-syntax-class DeleteFromClause
  #:attributes (table)
  (pattern (~seq #:from t:Name)
           #:attr table ($ t.ast)))

;; ============================================================
;; Table References && Expressions

(define-syntax-class TableRef
  #:attributes (ast)
  #:datum-literals (TableRef:AST TableRef:INJECT as)
  (pattern (TableRef:AST ~! u)
           #:declare u (UnquoteExpr/c #'table-ref?)
           #:attr ast ($ u.ast))
  (pattern (TableRef:INJECT ~! inj:StringOrUnquote)
           #:attr ast (table-ref:inject ($ inj.ast)))
  (pattern table-name:Name
           #:attr ast (table-ref:name ($ table-name.ast)))
  (pattern (as table-name:Name range-var:Ident)
           #:attr ast (table-ref:as (table-ref:name ($ table-name.ast))
                                    ($ range-var.ast)))
  (pattern (as ~! t:TableExpr range-var:Ident)
           #:attr ast (table-ref:as ($ t.ast) ($ range-var.ast)))
  (pattern :TableExpr))

(define-syntax-class TableExpr
  #:attributes (ast)
  #:datum-literals (TableExpr:AST TableExpr:INJECT cross-join values values*)
  (pattern (TableExpr:AST ~! u)
           #:declare u (UnquoteExpr/c #'table-expr?)
           #:attr ast ($ u.ast))
  (pattern (TableExpr:INJECT ~! inj:StringOrUnquote)
           #:attr ast (table-expr:inject ($ inj.ast)))
  (pattern (cross-join ~! t1:TableRef t2:TableRef)
           #:attr ast (table-expr:cross-join ($ t1.ast) ($ t2.ast)))
  (pattern (j:Join ~! t1:TableRef t2:TableRef :join-on-clause)
           #:attr ast (table-expr:join (syntax-e #'j) ($ t1.ast) ($ t2.ast) ($ on)))
  (pattern (so:SetOp ~! t1:expr t2:expr :maybe-all :set-op-clause)
           #:attr ast (table-expr:set-op (syntax-e #'so)
                                         (parse-table-expr #'t1)
                                         (parse-table-expr #'t2)
                                         (attribute all?)
                                         (attribute corr)))
  (pattern (values ~! e:expr ...)
           #:attr ast (table-expr:values
                       (list (map parse-scalar-expr (syntax->list #'(e ...))))))
  (pattern (values* ~! [e:expr ...] ...)
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
  (pattern (~seq #:corresponding-by column:Ident ...)
           #:attr corr (syntax->datum #'(column ...)))
  (pattern (~seq)
           #:attr corr #f))

(define-splicing-syntax-class maybe-all
  (pattern (~seq #:all) #:attr all? #t)
  (pattern (~seq #:all) #:attr all? #f))

(define-splicing-syntax-class join-on-clause
  (pattern (~seq #:natural)
           #:attr on '(natural))
  (pattern (~seq #:using column:id ...)
           #:attr on `(using ,(syntax->datum #'(column ...))))
  (pattern (~seq #:on condition:ScalarExpr)
           #:attr on `(on ,($ condition.ast))))

;; ============================================================
;; Scalar Expressions

(define-syntax-class ScalarExpr
  #:attributes (ast)
  #:datum-literals (ScalarExpr:AST ScalarExpr:INJECT ? unquote case)
  (pattern (ScalarExpr:AST ~! u)
           #:declare u (UnquoteExpr/c #'scalar-expr?)
           #:attr ast ($ u.ast))
  (pattern (ScalarExpr:INJECT ~! inj:StringOrUnquote)
           #:attr ast (scalar:inject ($ inj.ast)))
  (pattern (~and (case ~! . _) ce:CaseExpr)
           #:attr ast ($ ce.ast))
  (pattern (unquote ~! e:expr)
           #:attr ast (scalar:unquote #'e))
  (pattern n:exact-integer
           #:attr ast (syntax-e #'n))
  (pattern s:str
           #:attr ast (syntax-e #'s))
  (pattern :Name)
  (pattern ?
           #:attr ast (scalar:placeholder))
  (pattern te:TableExpr
           #:attr ast (scalar:table ($ te.ast)))
  (pattern (op:Op arg:ScalarExpr ...)
           #:fail-unless (check-arity ($ op.ast)
                                      (length (syntax->list #'(arg ...))))
                         "wrong arity"
           #:attr ast (scalar:app ($ op.ast) ($ arg.ast))))

(define-syntax-class Op
  #:attributes (ast)
  (pattern :NonSpecialId)
  (pattern :Name))

(define-syntax-class CaseExpr
  #:attributes (ast)
  #:datum-literals (case else)
  (pattern (case #:of ~! value:ScalarExpr cs:CaseClause ... [else ec:ScalarExpr])
           #:attr ast (scalar:case-of ($ value.ast) ($ cs.ast) ($ ec.ast)))
  (pattern (case cs:CaseClause ... [else ec:ScalarExpr])
           #:attr ast (scalar:case ($ cs.ast) ($ ec.ast))))

(define-syntax-class CaseClause
  #:attributes (ast)
  (pattern [q:ScalarExpr a:ScalarExpr]
           #:attr ast (cons ($ q.ast) ($ a.ast))))

;; ============================================================
;; Names and Identifiers

;; TODO:
;; - have mode where Racket identifier parsed as lit-id?
;; - ...?

;; Notes on SQL identifier syntax:
;; - Date & Darwen pp33-35
;; - PostgreSQL: http://www.postgresql.org/docs/8.2/static/sql-syntax-lexical.html
;; - SQLite: http://www.sqlite.org/lang_keywords.html
;; - MySQL: http://dev.mysql.com/doc/refman/5.0/en/identifiers.html

(define-syntax-class Name
  #:attributes (ast)
  #:datum-literals (Ident: Name:)
  (pattern x:id
           #:fail-when (special-symbol? (syntax-e #'x)) "reserved identifier"
           #:fail-when (regexp-match? #rx"--" (symbol->string (syntax-e #'x)))
                       "identifier includes SQL comment syntax"
           #:attr ast (symbol->name (syntax-e #'x))
           #:when ($ ast)) ;; FIXME: need better error message!
  (pattern (Ident: x:id)
           #:attr ast (syntax-e #'x))
  (pattern (Ident: x:str)
           #:attr ast (id:quoted (syntax-e #'x)))
  (pattern (Name: part:Name ...+)
           #:attr ast (name-list->name ($ part.ast))))

(define-syntax-class Ident
  #:attributes (ast)
  (pattern x:Name
           #:fail-when (qname? ($ x.ast)) "expected unqualified name"
           #:attr ast ($ x.ast)))

(define-syntax-class NonSpecialId
  #:attributes (ast)
  (pattern x:id
           #:fail-when (special-symbol? (syntax-e #'x)) "reserved identifier"
           #:fail-when (regexp-match? #rx"--" (symbol->string (syntax-e #'x)))
                       "identifier includes SQL comment syntax"
           #:attr ast (syntax-e #'x)))

(define (symbol->name s)
  (define parts (regexp-split #rx"\\." (symbol->string s)))
  (and (for/and ([part (in-list parts)])
         (SQL-regular-id? part))
       (symbol-list->name (map string->symbol parts))))

(define (symbol-list->name parts)
  (for/fold ([qual (car parts)]) ([part (in-list (cdr parts))])
    (qname qual part)))

(define (name-list->name ns)
  (define (prepend qual n)
    (match n
      [(qname qual* id)
       (qname (prepend qual qual*) id)]
      [_
       (qname qual n)]))
  (for/fold ([qual (car ns)]) ([n (in-list (cdr ns))])
    (prepend qual n)))

;; The following symbols are special in this library and are not
;; parsed as identifiers. Note: "special" overlaps with, but is not
;; the same as, "reserved word" in SQL.

(define (special-symbol? sym)
  (and (memq sym special-symbols) #t))

(define special-symbols
  '(? unquote     ;; these have other special meanings
    select insert update delete
    as values values* union except intersect
    cross-join inner-join left-join right-join full-join union-join
    from as where ;; to catch forgotten "#:" mistakes
    ))

(define (SQL-compound-regular-id? s)
  (define parts (regexp-split #rx"\\." s))
  (for/and ([part (in-list parts)])
    (SQL-regular-id? part)))


;; ============================================================
;; Other

(define-syntax-class (UnquoteExpr/c ctc)
  #:attributes (c ast)
  #:datum-literals (unquote)
  (pattern (unquote e)
           #:declare e (expr/c ctc)
           #:with c #'e.c
           #:attr ast (list 'unquote #'c)))

(define-syntax-class StringOrUnquote
  #:attributes (ast)
  #:datum-literals (unquote)
  (pattern (unquote e)
           #:declare e (expr/c #'string?)
           #:attr ast (list 'unquote #'e.c))
  (pattern s:str
           #:attr ast (syntax-e #'s)))
