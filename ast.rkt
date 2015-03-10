;; Run-time SQL ast structures

#lang racket/base
(require racket/string)
(provide (all-defined-out))

;; A UID (unqualified identifier) is one of
;;  - string                            ;; dots interpreted literally (if possible)
;;  - symbol                            ;; same

;; An ID is one of
;;  - string                            ;; dot-separated
;;  - symbol                            ;; dot-separated
;;  - (qid (listof (U UID)))  
(struct qid (parts) #:prefab)

;; qid* : UID ... -> ID
(define (qid* . parts)
  (qid parts))

;; symbol/string->qid : (U symbol string) -> qid
(define (symbol/string->qid s)
  (let ([s (if (symbol? s) (symbol->string s) s)])
    (qid (string-split s "." #:trim? #f))))

;; ----------------------------------------

;; A TableRef is one of
;;  - table-ID
;;  - TableExpr
;;  - (table:as (U table-ID TableExpr) range-UID (U #f (listof column-UID)))
(struct table:as (texpr range-var columns) #:prefab)

;; A TableExpr is one of
;;  - JoinTableExpr
;;  - (table:set-expr SetOp TableExpr TableExpr boolean SetSpec)
;;  - SelectExpr
;;  - (table:TABLE table-ID)
;;  - (table:VALUES (listof RowConstructor))
(struct table:set-expr (setop t1 t2 all? opt) #:prefab)
(struct table:TABLE (tid) #:prefab)
(struct table:VALUES (rows) #:prefab)

;; A SetSpec is one of
;;  - #f
;;  - 'corresponding-auto
;;  - (setspec:corresponding (listof column-???ID))
(struct setspec:corresponding (columns) #:prefab)

;; A JoinTableExpr is one of
;;  - (table:join JoinType TableExpr TableExpr JoinOnSpec)
(struct table:join (type t1 t2 spec) #:prefab)

;; A JoinType is one of 'inner | 'left | 'right | 'full | 'cross | 'union
;; A JoinOnSpec is one of
;;  - #f                                ;; iff JoinType is 'cross or 'union
;;  - 'natural
;;  - (join:on cond-ScalarExpr)
;;  - (join:using (listof column-UID))
(struct join:on (condition) #:prefab)
(struct join:using (columns) #:prefab)

;; A SelectExpr is
;;  (table:select (listof TableRef) (listof SelectItem) (listof ScalarExpr)
;;                (U #f (listof column-ID)) (listof ScalarExp))
(struct table:select (values from where group-by having) #:prefab)

;; A RowConstructor is one of
;;  - (row:tuple (listof ScalarExpr))
;;  - (row:query1 TableExpr)
(struct row:tuple (exprs) #:prefab)
(struct row:query1 (texpr) #:prefab)

;; ============================================================

;; A ScalarExpr is one of
;;  - (se:literal Any ScalarType)
;;  - (se:app Operator (listof Argument))
;;  -


(struct se:literal (value type) #:prefab)
(struct se:app (op args) #:prefab)


;; An Operator is
;;  (operator String Formatter (listof Kind) Kind)
(struct operator (name formatter arg-kinds result-type))

;; A Kind is one of
;;  - ScalarType
;;  - 'any-scalar
;;  - 'type
;;  - 'row
;;  - 'table
;;  - ???

;; An Argument is one of
;;  - ScalarExpr
;;  - (arg:type ScalarType)
;;  - (arg:special String)
(struct arg:type (type) #:prefab)
(struct arg:special (sql) #:prefab)

;; eg "CAST(x as TEXT)" <-> (se:app CAST-op (list (se:varref 'x) (se:type "TEXT")))

