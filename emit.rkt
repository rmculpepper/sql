;; Translate AST to SQL

#lang racket/base
(require racket/string
         racket/list
         (rename-in racket/match [match-define defmatch])
         racket/format
         "ast.rkt")
(provide (all-defined-out))

;; ============================================================
;; Util: jumbles

;; A Jumble is String | (Listof Jumble)
(define (J . args) args)
(define (J-join js sep) (add-between js sep))

(define (jumble->string j)
  (define out (open-output-string))
  (let loop ([j j])
    (cond [(string? j) (write-string j out)]
          [else (for-each loop j)]))
  (get-output-string out))

;; ============================================================
;; Emit according to concrete syntax for minimal parenthesization.

(define (select->string s)
  (jumble->string (emit-select s)))
(define (table-ref->string t)
  (jumble->string (emit-table-ref t)))
(define (table-expr->string t)
  (jumble->string (emit-table-expr t)))
(define (scalar-expr->string e)
  (jumble->string (emit-scalar-expr e)))

;; ----------------------------------------

(define (emit-select s)
  (match s
    [(stmt:select vals froms wheres)
     (J "SELECT "
        (J-join (map emit-select-item vals) ", ")
        (if (pair? froms)
            (J " FROM " (J-join (map emit-table-ref froms) ", "))
            "")
        (if (pair? wheres)
            (J " WHERE " (J-join (map emit-scalar-expr wheres) " AND "))
            ""))]))

(define (emit-select-item si)
  (match si
    [(select-item:as expr var)
     (J (emit-scalar-expr expr) " AS " (emit-id var))]
    [_ (emit-scalar-expr si)]))

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
           (J " USING (" (J-join (map emit-id columns) ", ") ")")]
          [`(on ,condition)
           (J " ON " (emit-scalar-expr condition))]
          [_ ""]))]))

(define (emit-table-ref t)
  (match t
    [(table-ref:id table-name)
     (emit-id table-name)]
    [(table-ref:as (table-ref:id table-name) rangevar)
     (J (emit-id table-name) " AS " (emit-id rangevar))]
    [(table-ref:as table-expr rangevar)
     (J "(" (emit-table-expr table-expr) ") AS " (emit-id rangevar))]
    [(? join-table-expr?)
     (emit-join-table-expr t)]
    [_
     (eprintf "WARNING: may be illegal\n")
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
    [_ (emit-nonjoin-table-term t)]))

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
        (string-join
         (for/list ([row rows])
           (J "(" (J-join (map emit-scalar-expr row) ", ") ")"))
         ", "))]
    [(table-expr:select select)
     (error 'unimplemented)]
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
        (~a "CORRESPONDING (" (string-join columns ", ") ") ")])))

;; ----------------------------------------

(define (emit-scalar-expr e)
  (match e
    [(scalar:app (op _ formatter) args)
     (apply formatter (map emit-scalar-expr args))]
    [(scalar:placeholder)
     "?"]
    [(scalar:literal s)
     s]
    [(? symbol?)
     (~s e)]
    [(? string?)
     (~s e)]
    [(? exact-integer?)
     (~s e)]))

;; ----------------------------------------

(define (emit-id id) (~a id))
