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
    [x:With   ($ x.ast)]
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
  #:attributes (ast) #:commit
  (pattern :With)
  (pattern :Select)
  (pattern :Insert)
  (pattern :Update)
  (pattern :Delete))

(define-syntax-class With
  #:attributes (ast) #:commit
  (pattern (~and ((~datum with) . _) :WithInner)))
(define-syntax-class Select
  #:attributes (ast) #:commit
  (pattern (~and ((~datum select) . _) :SelectInner)))
(define-syntax-class Insert
  #:attributes (ast) #:commit
  (pattern (~and ((~datum insert) . _) :InsertInner)))
(define-syntax-class Update
  #:attributes (ast) #:commit
  (pattern (~and ((~datum update) . _) :UpdateInner)))
(define-syntax-class Delete
  #:attributes (ast) #:commit
  (pattern (~and ((~datum delete) . _) :DeleteInner)))

;; ============================================================
;; DDL Statements

(define-syntax-class DDL
  #:attributes (ast) #:commit
  (pattern :CreateTable)
  (pattern :CreateView))

(define-syntax-class CreateTable
  #:attributes (ast) #:commit
  (pattern (~and ((~datum create-table) ~! . _) :CreateTableInner)))

(define-syntax-class CreateTableInner
  #:attributes (ast) #:commit
  (pattern (_ temp:MaybeTemporary ine:MaybeIfNotExists
              name:Name
              #:columns c:ColumnDef ...
              tc:TableConstraints)
           #:attr ast (ddl:create-table ($ name.ast) ($ temp.?) ($ ine.?)
                                        ($ c.ast) ($ tc.ast)))
  (pattern (_ temp:MaybeTemporary ine:MaybeIfNotExists
              name:Name #:as s:Statement)
           #:attr ast (ddl:create-table-as ($ name.ast) ($ temp.?) ($ ine.?) ($ s.ast))))

(define-splicing-syntax-class MaybeTemporary
  #:attributes (?)
  (pattern (~seq #:temporary) #:attr ? #t)
  (pattern (~seq) #:attr ? #f))

(define-splicing-syntax-class MaybeIfNotExists
  #:attributes (?)
  (pattern (~seq #:if-not-exists) #:attr ? #t)
  (pattern (~seq) #:attr ? #f))

(define-syntax-class ColumnDef
  #:attributes (ast) #:commit
  (pattern [name:Ident
            type:ScalarExpr
            (~alt (~optional (~and #:not-null nn))
                  (~optional (~seq #:default default:ScalarExpr)))
            ...]
           #:attr ast (column ($ name.ast) ($ type.ast)
                              (and ($ nn) #t) ($ default.ast))))

(define-splicing-syntax-class TableConstraints
  #:attributes ([ast 1])
  #:description #f
  (pattern (~seq)
           #:attr (ast 1) null)
  (pattern (~seq #:constraints c:TableConstraint ...)
           #:fail-when (let ([pks (for/list ([c (in-list (syntax->list #'(c ...)))]
                                             [pk? (in-list ($ c.pk?))]
                                             #:when pk?)
                                    c)])
                         (if (> (length pks) 1) (cadr pks) #f))
                       "duplicate primary key constraint"
           #:attr (ast 1) ($ c.ast)))

(define-syntax-class TableConstraint
  #:attributes (ast pk?) #:commit
  #:datum-literals (constraint)
  (pattern (constraint name:Ident c:TableConstraintInner)
           #:attr ast (constraint:named ($ name.ast) ($ c.ast))
           #:attr pk? ($ c.pk?))
  (pattern :TableConstraintInner))

(define-syntax-class TableConstraintInner
  #:attributes (ast pk?) #:commit
  #:datum-literals (primary-key unique check foreign-key)
  (pattern (primary-key c:Ident ...)
           #:attr ast (constraint:primary-key ($ c.ast))
           #:attr pk? #t)
  (pattern (unique c:Ident ...)
           #:attr ast (constraint:unique ($ c.ast))
           #:attr pk? #f)
  (pattern (check e:ScalarExpr)
           #:attr ast (constraint:check ($ e.ast))
           #:attr pk? #f)
  (pattern (foreign-key c:Ident ... #:references f:TableWColumns
                        (~alt (~optional (~seq #:on-delete delete:Action))
                              (~optional (~seq #:on-update update:Action)))
                        ...)
           #:attr ast (constraint:references ($ c.ast) (car ($ f.ast)) (cdr ($ f.ast))
                                             ($ delete.ast) ($ update.ast))
           #:attr pk? #f))

(define-syntax-class Action
  #:attributes (ast) #:commit
  (pattern (~and k (~or #:set-null #:set-default #:cascade #:restrict #:no-action))
           #:attr ast (string->symbol (keyword->string (syntax->datum #'k)))))

(define-syntax-class CreateView
  #:attributes (ast) #:commit
  (pattern (~and ((~datum create-view) ~! . _) :CreateViewInner)))

(define-syntax-class CreateViewInner
  #:attributes (ast) #:commit
  (pattern (_ (~optional (~and #:temporary temp?)) name:Name s:Statement)
           #:attr ast (ddl:create-view ($ name.ast) (and ($ temp?) #t) ($ s.ast))))


;; ============================================================
;; With Statement

(define-syntax-class WithInner
  #:attributes (ast) #:commit
  #:description #f
  (pattern (_ rec:MaybeRec ([h:TableWColumns (~or rhs:TableExpr rhs:Statement)] ...) body:Statement)
           #:attr ast (statement:with ($ rec.ast) ($ h.ast) ($ rhs.ast) ($ body.ast))))

(define-splicing-syntax-class MaybeRec
  #:attributes (ast)
  #:description #f
  (pattern (~seq #:recursive) #:attr ast #t)
  (pattern (~seq) #:attr ast #f))

(define-syntax-class TableWColumns
  #:attributes (ast) #:commit
  (pattern name:Ident
           #:attr ast (cons ($ name.ast) #f))
  (pattern (name:Ident column:Ident ...)
           #:attr ast (cons ($ name.ast) ($ column.ast))))

;; ============================================================
;; Select Statement

(define-syntax-class SelectInner
  #:attributes (ast) #:commit
  #:description #f
  (pattern (_ (~optional distinct:DistinctClause)
              vs:SelectValues
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
           #:attr ast (statement:select
                       ($ distinct.ast)
                       (append ($ vs.ast) (or ($ sel.ast) null))
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
  #:description #f
  (pattern (~seq :SelectItem ...)))

(define-splicing-syntax-class DistinctClause
  #:attributes (ast)
  #:description #f
  (pattern (~seq #:all) #:attr ast 'all)
  (pattern (~seq #:distinct) #:attr ast 'distinct))

(define-splicing-syntax-class SelectValuesClause
  #:attributes ([ast 1] kw)
  (pattern (~seq (~and #:values kw) :SelectValues)))

(define-syntax-class SelectItem
  #:attributes (ast) #:commit
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
  #:attributes (ast) #:commit
  #:description #f
  (pattern (_ target:InsertTarget
              assign:AssignClause
              on:OnConflict)
           #:attr ast (statement:insert
                       ($ target.ast)
                       (map update:assign-column ($ assign.ast))
                       (table-expr:values
                        (list (map update:assign-expr ($ assign.ast))))
                       ($ on.ast)))
  (pattern (_ target:InsertTarget
              (~optional cols:InsertColumns)
              src:InsertSource
              on:OnConflict)
           #:attr ast (statement:insert ($ target.ast) ($ cols.ast) ($ src.ast) ($ on.ast))))

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

(define-splicing-syntax-class OnConflict
  #:attributes (ast)
  (pattern (~seq #:or-ignore)
           #:attr ast 'ignore)
  (pattern (~seq)
           #:attr ast #f))

;; ============================================================
;; Update Statement

(define-syntax-class UpdateInner
  #:attributes (ast) #:commit
  #:description #f
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
  #:attributes (ast) #:commit
  (pattern [c:Ident e:ScalarExpr]
           #:attr ast (update:assign ($ c.ast) ($ e.ast))))

;; ============================================================
;; Delete Statement

(define-syntax-class DeleteInner
  #:attributes (ast) #:commit
  #:description #f
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
  #:attributes (ast) #:commit
  #:datum-literals (TableRef:AST TableRef:INJECT as)
  (pattern (TableRef:AST ~! u)
           #:declare u (UnquoteExpr/c #'table-ref-ast?)
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
  #:attributes (ast) #:commit
  #:datum-literals (TableExpr:AST TableExpr:INJECT cross-join values values* select)
  (pattern (TableExpr:AST ~! u)
           #:declare u (UnquoteExpr/c #'table-expr-ast?)
           #:attr ast ($ u.ast))
  (pattern (TableExpr:INJECT ~! inj:StringOrUnquote)
           #:attr ast (table-expr:inject ($ inj.ast)))
  (pattern (cross-join ~! t1:TableRef t2:TableRef)
           #:attr ast (table-expr:cross-join ($ t1.ast) ($ t2.ast)))
  (pattern (j:Join ~! t1:TableRef t2:TableRef :join-on-clause)
           #:attr ast (table-expr:join (syntax-e #'j) ($ t1.ast) ($ t2.ast) ($ on)))
  (pattern (so:SetOp ~! t1:TableExpr t2:TableExpr :maybe-all :set-op-clause)
           #:attr ast (table-expr:set-op (syntax-e #'so) ($ t1.ast) ($ t2.ast)
                                         (attribute all?) (attribute corr)))
  (pattern (values ~! e:ScalarExpr ...)
           #:attr ast (table-expr:values (list ($ e.ast))))
  (pattern (values* ~! [e:ScalarExpr ...] ...)
           #:do [(define l-rows ($ e.ast))]
           #:fail-unless (check-same-length l-rows)
           "values*: all rows must be the same length"
           #:attr ast (table-expr:values l-rows))
  (pattern (~and (select ~! . _) s:Select)
           ;; was just s:Select, but this gives better errors
           #:attr ast (table-expr:select ($ s.ast))))

(define-syntax-class Join
  #:commit
  #:description #f  ;; interferes with stxparse error-collapsing
  (pattern (~datum inner-join))
  (pattern (~datum left-join))
  (pattern (~datum right-join))
  (pattern (~datum full-join)))
(define-syntax-class SetOp
  #:commit
  #:description #f  ;; interferes with stxparse error-collapsing
  (pattern (~datum union))
  (pattern (~datum intersect))
  (pattern (~datum except)))

(define-splicing-syntax-class set-op-clause
  (pattern (~seq #:corresponding)
           #:attr corr 'auto)
  (pattern (~seq #:corresponding-by column:Ident ...)
           #:attr corr ($ column.ast))
  (pattern (~seq)
           #:attr corr #f))

(define-splicing-syntax-class maybe-all
  (pattern (~seq #:all) #:attr all? #t)
  (pattern (~seq) #:attr all? #f))

(define-splicing-syntax-class join-on-clause
  (pattern (~seq #:natural)
           #:attr on '(natural))
  (pattern (~seq #:using column:Ident ...)
           #:attr on `(using ,($ column.ast)))
  (pattern (~seq #:on condition:ScalarExpr)
           #:attr on `(on ,($ condition.ast))))

;; ============================================================
;; Scalar Expressions

(define-syntax-class ScalarExpr
  #:attributes (ast) #:commit
  #:description "scalar expression"
  #:datum-literals (ScalarExpr:AST ScalarExpr:INJECT ? unquote case exists in)
  (pattern (ScalarExpr:AST ~! (~var u (UnquoteExpr/c #'scalar-expr-ast?)))
           #:attr ast ($ u.ast))
  (pattern (ScalarExpr:INJECT ~! inj:StringOrUnquote)
           #:attr ast (scalar:inject ($ inj.ast)))
  ;; ----
  (pattern n:exact-integer              #:attr ast (syntax-e #'n))
  (pattern s:str                        #:attr ast (syntax-e #'s))
  (pattern ?                            #:attr ast (scalar:placeholder))
  (pattern (unquote ~! e:expr)          #:attr ast (scalar:unquote (list 'unquote #'e)))
  (pattern :Name)
  ;; ----
  (pattern (exists ~! te:TableExpr)
           #:attr ast (scalar:exists ($ te.ast)))
  (pattern (~and (in ~! . _) :ScalarExpr/In))
  (pattern (~and (case ~! . _) :ScalarExpr/Case))
  (pattern te:TableExpr
           #:attr ast (scalar:table ($ te.ast)))
  (pattern (op:OperatorSymbol e1:ScalarExpr #:some ~! (~or e2:TableExpr e2:ScalarExpr))
           #:attr ast (scalar:some/all ($ op.ast) ($ e1.ast) 'some ($ e2.ast)))
  (pattern (op:OperatorSymbol e1:ScalarExpr #:all ~! (~or e2:TableExpr e2:ScalarExpr))
           #:attr ast (scalar:some/all ($ op.ast) ($ e1.ast) 'all ($ e2.ast)))
  (pattern (op:Operator ~! arg:ScalarExpr ...)
           #:fail-unless (arity-includes? ($ op.arity) (length (syntax->list #'(arg ...))))
                         (format "wrong arity for operator or special function\n  expected: ~a"
                                 (arity->string ($ op.arity)))
           #:attr ast (scalar:app ($ op.ast) ($ arg.ast)))
  (pattern (op:Name arg:ScalarExpr ...)
           #:attr ast (scalar:app ($ op.ast) ($ arg.ast))))

(define-syntax-class ScalarExpr/Case
  #:attributes (ast) #:commit
  #:description "CASE scalar expression"
  #:datum-literals (case else)
  (pattern (case #:of ~! value:ScalarExpr cs:CaseClause ... [else ec:ScalarExpr])
           #:attr ast (scalar:case-of ($ value.ast) ($ cs.ast) ($ ec.ast)))
  (pattern (case cs:CaseClause ... [else ec:ScalarExpr])
           #:attr ast (scalar:case ($ cs.ast) ($ ec.ast))))

(define-syntax-class CaseClause
  #:attributes (ast) #:commit
  (pattern [q:ScalarExpr a:ScalarExpr]
           #:attr ast (cons ($ q.ast) ($ a.ast))))

(define-syntax-class ScalarExpr/In
  #:attributes (ast) #:commit
  #:description "IN scalar expression"
  (pattern (_ e1:ScalarExpr #:from e2:TableExpr)
           #:attr ast (scalar:in-table ($ e1.ast) ($ e2.ast)))
  (pattern (_ e1:ScalarExpr #:values e2:ScalarExpr ...)
           #:attr ast (scalar:in-values ($ e1.ast) ($ e2.ast))))

;; ============================================================
;; Names and Identifiers

;; Notes on SQL identifier syntax:
;; - Date & Darwen pp33-35
;; - PostgreSQL: http://www.postgresql.org/docs/8.2/static/sql-syntax-lexical.html
;; - SQLite: http://www.sqlite.org/lang_keywords.html
;; - MySQL: http://dev.mysql.com/doc/refman/5.0/en/identifiers.html

(define-syntax-class UntaggedIdent
  #:attributes (ast) #:commit
  #:description #f
  (pattern x:id
           #:fail-when (special-symbol? (syntax-e #'x))
                       "special symbol cannot be used as untagged identifier"
           #:attr ast (parse-name (syntax-e #'x))
           #:fail-unless ($ ast) "illegal character in untagged identifier"))

(define-syntax-class TaggedIdent
  #:attributes (ast) 
  #:datum-literals (Ident: Ident:AST)
  #:description #f
  (pattern (Ident: x:id)
           #:attr ast (id:normal (syntax-e #'x)))
  (pattern (Ident: x:str)
           #:attr ast (id:quoted (syntax-e #'x)))
  (pattern (Ident:AST ~! u)
           #:declare u (UnquoteExpr/c #'ident-ast?)
           #:attr ast ($ u.ast)))

(define-syntax-class Ident
  #:attributes (ast) #:commit
  (pattern x:UntaggedIdent
           #:fail-when (qname? ($ x.ast)) "expected unqualified name"
           #:attr ast ($ x.ast))
  (pattern x:TaggedIdent
           #:attr ast ($ x.ast)))

(define-syntax-class Name
  #:attributes (ast) #:commit
  #:datum-literals (Name: Name:AST)
  (pattern x:UntaggedIdent
           #:attr ast ($ x.ast))
  (pattern x:TaggedIdent
           #:attr ast ($ x.ast))
  (pattern (Name:AST ~! u)
           #:declare u (UnquoteExpr/c #'name-ast?)
           #:attr ast ($ u.ast))
  (pattern (Name: part:Name ...+)
           #:attr ast (name-list->name ($ part.ast))))



(define-syntax-class Operator
  #:attributes (ast arity) #:commit
  #:description #f
  (pattern x:id
           #:attr arity (op-arity (syntax-e #'x))
           #:when ($ arity)
           ;; "--" check should be redundant
           #:fail-when (regexp-match? #rx"--" (symbol->string (syntax-e #'x)))
                       "operator includes SQL comment syntax"
           #:attr ast (syntax-e #'x)))

(define-syntax-class OperatorSymbol
  #:attributes (ast) #:commit
  (pattern x:id
           #:when (operator-symbol? (syntax-e #'x))
           ;; "--" check should be redundant
           #:fail-when (regexp-match? #rx"--" (symbol->string (syntax-e #'x)))
                       "operator includes SQL comment syntax"
           #:attr ast (syntax-e #'x)))

;; ------------------------------------------------------------
(module names racket/base
(require racket/match
         "ast.rkt")
(provide (all-defined-out))

(define (parse-name s)
  (cond [(symbol? s)
         (parse-name (symbol->string s))]
        [else
         (define parts (regexp-split #rx"\\." s))
         (and (for/and ([part (in-list parts)])
                (ok-id-part? part))
              (symbol-list->name (map string->symbol parts)))]))

;; ok-id-part? : String -> Boolean
(define (ok-id-part? s)
  (regexp-match? #px"^(?:\\p{L}|_)(?:\\p{L}|\\p{N}|[_$])*$" s))

(define (symbol-list->name parts)
  (let ([parts (map id:normal parts)])
    (for/fold ([qual (car parts)]) ([part (in-list (cdr parts))])
      (qname qual part))))

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
;; parsed as identifiers. Every datum-literal should appear in the
;; list, as well as symbol versions of the main keywords.  It's okay
;; if a dotted identifier contains a special word: "select" is
;; special, but "select.as" is okay.

;; Note: "special" is independent of "reserved keyword" (although they
;; overlap, because of the design of the libary). Reserved words are
;; handled by dialect.rkt.

(define (special-symbol? sym)
  (hash-ref special-symbols-table sym #f))

(define special-symbols
  '(with select insert update delete create-table create-view
    temporary constraint primary-key unique check foreign-key references
    as * from where values values* group-by having order-by asc desc limit offset
    set columns into
    cross-join inner-join left-join right-join full-join union intersect except
    all some distinct corresponding corresponding-by natural using on
    ? unquote case of else exists in 

    TableRef:AST TableRef:INJECT
    TableExpr:AST TableExpr:INJECT
    ScalarExpr:AST ScalarExpr:INJECT
    Ident:AST Name:AST
    Ident: Name:))

(define special-symbols-table
  (for/hash ([w (in-list special-symbols)]) (values w #t)))
)
(require 'names)
;; ------------------------------------------------------------

;; ============================================================
;; Other

(define-syntax-class (UnquoteExpr/c ctc)
  #:attributes (c ast) #:commit
  #:datum-literals (unquote)
  (pattern (unquote e)
           #:declare e (expr/c ctc)
           #:with c #'e.c
           #:attr ast (list 'unquote #'c)))

(define-syntax-class StringOrUnquote
  #:attributes (ast) #:commit
  #:datum-literals (unquote)
  (pattern (unquote e)
           #:declare e (expr/c #'string?)
           #:attr ast (list 'unquote #'e.c))
  (pattern s:str
           #:attr ast (syntax-e #'s)))
