;; Translate AST to SQL

#lang racket/base
(require racket/string
         (rename-in racket/match [match-define defmatch])
         racket/format
         "ast.rkt")
(provide (all-defined-out))

(define (emit-table-ref t)
  (match t
    [(table-ref:id table-name)
     (emit-id table-name)]
    [(table-ref:as (table-ref:id table-name) rangevar)
     (~a (emit-id table-name) " AS " (emit-id rangevar))]
    [(table-ref:as table-expr rangevar)
     (~a "(" (emit-table-expr table-expr) ") AS " (emit-id rangevar))]
    [(? table-expr?)
     (~a "(" (emit-table-expr t) ")")]))

(define (emit-table-expr t)
  (match t
    [(table-expr:cross-join t1 t2)
     (~a (emit-table-ref t1)
         " CROSS JOIN "
         (emit-table-ref t2))]
    [(table-expr:join type t1 t2 on)
     (~a "("
         (emit-table-ref t1)
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
            (~a " USING (" (string-join (map emit-id columns) ",") ")")]
           [`(on ,condition)
            (~a " ON " (emit-scalar-expr condition))]
           [_ ""])
         ")")]
    [(table-expr:set-op type t1 t2 opt corr)
     (~a "("
         (emit-table-ref t1)
         (case type
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
            (~a "CORRESPONDING (" (string-join columns ",") ") ")])
         (emit-table-ref t2)
         ")")]
    [(table-expr:values rows)
     (~a "VALUES "
         (string-join
          (for/list ([row rows])
            (~a "(" (string-join (map emit-scalar-expr row) ",") ")"))
          ","))]
    [(table-expr:select select)
     (error 'unimplemented)]))

(define (emit-scalar-expr e)
  (match e
    [(scalar:app (op _ formatter) args)
     (apply formatter args)]
    [(? symbol?)
     (~s e)]
    [(? string?)
     (~s e)]
    [(? exact-integer?)
     (~s e)]))
