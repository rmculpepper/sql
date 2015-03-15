;; Translate AST to SQL

#lang racket/base
(require racket/string
         racket/list
         (rename-in racket/match [match-define defmatch])
         racket/format
         "ast.rkt"
         "jumble.rkt")
(provide (all-defined-out))

;; ============================================================
;; Emit according to concrete syntax for minimal parenthesization.

(define (statement->string s)
  (jumble->string (emit-statement s)))
(define (table-ref->string t)
  (jumble->string (emit-table-ref t)))
(define (table-expr->string t)
  (jumble->string (emit-table-expr t)))
(define (scalar-expr->string e)
  (jumble->string (emit-scalar-expr e)))

;; ----------------------------------------

(define (emit-statement s)
  (cond [(stmt:select? s) (emit-select s)]
        [(stmt:insert? s) (emit-insert s)]
        [(stmt:update? s) (emit-update s)]
        [(stmt:delete? s) (emit-delete s)]))

;; ----------------------------------------

(define (emit-select s)
  (match s
    [(stmt:select vals froms wheres groupby having ext)
     (J "SELECT "
        (J-join (map emit-select-item vals) ", ")
        (if (pair? froms)
            (J " FROM " (J-join (map emit-table-ref froms) ", "))
            "")
        (if (pair? wheres)
            (J " WHERE " (J-join (map emit-scalar-expr wheres) " AND "))
            "")
        (if (pair? groupby)
            (J " GROUP BY " (J-join (map emit-name groupby) ", "))
            "")
        (if (pair? having)
            (J " HAVING " (J-join (map emit-scalar-expr having) " AND "))
            "")
        (match ext
          [(select:extension order limit offset)
           (J (if order
                  (J " ORDER BY " (J-join (map emit-select-order order) ", "))
                  "")
              (if limit
                  (J " LIMIT " (emit-scalar-expr limit))
                  "")
              (if offset
                  (J " OFFSET " (emit-scalar-expr offset))
                  ""))]
          [#f ""]))]))

(define (emit-select-item si)
  (match si
    [(select-item:as expr var)
     (J (emit-scalar-expr expr) " AS " (emit-ident var))]
    [(select-item:all)
     "*"]
    [_ (emit-scalar-expr si)]))

(define (emit-select-order so)
  (match so
    [(select:order column asc/desc)
     (J (emit-name column)
        (case asc/desc
          [(asc) " ASC"]
          [(desc) " DESC"]
          [(#f) ""]))]))

;; ----------------------------------------

(define (emit-insert i)
  (match i
    [(stmt:insert table columns source)
     (J "INSERT INTO "
        (emit-name table)
        (if columns
            (J " (" (J-join (map emit-ident columns) ", ") ") ")
            " ")
        (emit-table-expr source))]))

;; ----------------------------------------

(define (emit-update u)
  (match u
    [(stmt:update table assign where)
     (J "UPDATE "
        (emit-name table)
        " SET "
        (J-join (map emit-update-assign assign) ", ")
        (if (pair? where)
            (J " WHERE " (J-join (map emit-scalar-expr where) " AND "))
            ""))]))

(define (emit-update-assign a)
  (match a
    [(update:assign column expr)
     (J (emit-ident column) " = " (emit-scalar-expr expr))]))

;; ----------------------------------------

(define (emit-delete d)
  (match d
    [(stmt:delete table where)
     (J "DELETE FROM "
        (emit-name table)
        (if (pair? where)
            (J " WHERE " (J-join (map emit-scalar-expr where) " AND "))
            ""))]))

;; ----------------------------------------

(define (emit-table-expr t)
  (cond [(join-table-expr? t)
         (emit-join-table-expr t)]
        [(nonjoin-table-expr? t)
         (emit-nonjoin-table-expr t)]))

(define (emit-join-table-expr t)
  (match t
    [(table-expr:cross-join t1 t2)
     (J (emit-table-ref t1)
        " CROSS JOIN "
        (emit-table-ref t2))]
    [(table-expr:join type t1 t2 on)
     (J (emit-table-ref t1)
        (match on
          [`(natural) " NATURAL"]
          [_""])
        (case type
          [(inner-join) " INNER JOIN "]
          [(left-join)  " LEFT OUTER JOIN "]
          [(right-join) " RIGHT OUTER JOIN "]
          [(full-join)  " FULL OUTER JOIN "]
          [(union-join) " UNION JOIN "])
        (emit-table-ref t2)
        (match on
          [`(using ,columns)
           (J " USING (" (J-join (map emit-ident columns) ", ") ")")]
          [`(on ,condition)
           (J " ON " (emit-scalar-expr condition))]
          [_ ""]))]))

(define (emit-table-ref t)
  (match t
    [(table-ref:name table-name)
     (emit-name table-name)]
    [(table-ref:as (table-ref:name table-name) rangevar)
     (J (emit-name table-name) " AS " (emit-ident rangevar))]
    [(table-ref:as table-expr rangevar)
     (J "(" (emit-table-expr table-expr) ") AS " (emit-ident rangevar))]
    [(? join-table-expr?)
     (emit-join-table-expr t)]
    [_
     (eprintf "WARNING: may be illegal syntax\n")
     (J "(" (emit-table-expr t) ")")]))

(define (emit-nonjoin-table-expr t)
  (match t
    [(table-expr:set-op (and type (or 'union 'except)) t1 t2 opt corr)
     (J (emit-table-expr t1)
        (emit-set-op-parts type opt corr)
        (emit-table-term t2))]
    [_ (emit-nonjoin-table-term t)]))

(define (emit-table-term t)
  (cond [(join-table-expr? t)
         (emit-join-table-expr t)]
        [else
         (emit-nonjoin-table-term t)]))

(define (emit-nonjoin-table-term t)
  (match t
    [(table-expr:set-op (and type 'intersect) t1 t2 opt corr)
     (J (emit-table-term t1)
        (emit-set-op-parts type opt corr)
        (emit-table-primary t2))]
    [_ (emit-nonjoin-table-primary t)]))

(define (emit-table-primary t)
  (cond [(join-table-expr? t)
         (emit-join-table-expr t)]
        [else
         (emit-nonjoin-table-primary t)]))

(define (emit-nonjoin-table-primary t)
  (match t
    ;; [(table-expr:table ...) ...] ;; "TABLE table-name"
    [(table-expr:values rows)
     (J "VALUES "
        (J-join
         (for/list ([row rows])
           (J "(" (J-join (map emit-scalar-expr row) ", ") ")"))
         ", "))]
    [(table-expr:select select)
     (emit-select select)]
    [_ (J "(" (emit-table-expr t) ")")]))

(define (emit-set-op-parts type opt corr)
  (J (case type
       [(union) " UNION "]
       [(except) " EXCEPT "]
       [(intersect) " INTERSECT "])
     (case opt
       [(all) "ALL "]
       [else ""])
     (match corr
       [`#f ""]
       [`#t "CORRESPONDING "]
       [(list columns ...)
        (J "CORRESPONDING (" (J-join (map emit-ident columns) ", ") ") ")])))

;; ----------------------------------------

(define (emit-scalar-expr e)
  (match e
    [(scalar:app op args)
     (define formatter (or (op-formatter op) (fun-op (emit-name op))))
     (apply formatter (map emit-scalar-expr args))]
    [(scalar:placeholder)
     "?"]
    [(scalar:literal s)
     s]
    [(or (? symbol?) (? qname?))
     (emit-name e)]
    [(? string?)
     (J "'" (regexp-replace* #rx"'" e "''") "'")]
    [(? exact-integer?)
     (number->string e)]))

;; ----------------------------------------

(define (emit-name n)
  (match n
    [(qname qual id)
     (J (emit-name qual) "." (emit-ident id))]
    [_ (emit-ident n)]))

(define (emit-ident id)
  (match id
    [(id:literal (? string? s))
     (J "\"" (regexp-replace* #rx"\"" s "\"\"") "\"")]
    [(? symbol? s)
     (symbol->string s)]))
