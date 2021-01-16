;; Translate AST to SQL

#lang racket/base
(require racket/string
         racket/include
         racket/class
         racket/list
         (rename-in racket/match [match-define defmatch])
         racket/format
         "ast.rkt"
         "dialect.rkt"
         "jumble.rkt")
(provide (all-defined-out))

;; ----------------------------------------

;; So we can use map with method names
(define-syntax-rule (map f xs) (for/list ([x (in-list xs)]) (f x)))

(define next-dollar-placeholder (make-parameter 1))

;; ----------------------------------------

(define emit-sql%
  (class object%
    (init-field dialect)
    (super-new)

    ;; ----------------------------------------
    ;; Entry points (to set up state)

    (define/public (call-as-entry f)
      (parameterize ((next-dollar-placeholder 1))
        (f)))

    ;; ----------------------------------------
    ;; Convenience entry points

    (define/public (statement->string s)
      (jumble->string (call-as-entry (lambda () (emit-statement s)))))
    (define/public (table-ref->string t)
      (jumble->string (call-as-entry (lambda () (emit-table-ref t)))))
    (define/public (table-expr->string t)
      (jumble->string (call-as-entry (lambda () (emit-table-expr t)))))
    (define/public (scalar-expr->string e)
      (jumble->string (call-as-entry (lambda () (emit-scalar-expr e)))))
    (define/public (name->string e)
      (jumble->string (call-as-entry (lambda () (emit-name e)))))
    (define/public (ident->string e)
      (jumble->string (call-as-entry (lambda () (emit-ident e)))))

    ;; ----------------------------------------
    ;; Statements

    (define/public (emit-statement s)
      (match s
        [(? statement:with?)   (emit-with s)]
        [(? statement:select?) (emit-select s)]
        [(? statement:insert?) (emit-insert s)]
        [(? statement:update?) (emit-update s)]
        [(? statement:delete?) (emit-delete s)]
        [(? ddl:create-table?) (emit-create-table s)]
        [(? ddl:create-table-as?) (emit-create-table-as s)]
        [(? ddl:create-view?) (emit-create-view s)]))

    ;; ----------------------------------------
    ;; DDL Statements

    (define/public (emit-create-view s)
      (match s
        [(ddl:create-view name temp? rhs)
         (J "CREATE " (if temp? "TEMPORARY " "") "VIEW " (emit-name name)
            " AS " (emit-statement rhs))]))

    (define/public (emit-create-table s)
      (match s
        [(ddl:create-table name temp? ifnotexists? columns constraints)
         (J "CREATE " (if temp? "TEMPORARY " "") "TABLE "
            (if ifnotexists? "IF NOT EXISTS " "")
            (emit-name name)
            " (" (J-join (map emit-column columns) ", ")
            (if (and (pair? columns) (pair? constraints)) ", " "")
            (J-join (map emit-constraint constraints) ", ")
            ")")]))

    (define/public (emit-create-table-as s)
      (match s
        [(ddl:create-table-as name temp? ifnotexists? rhs)
         (J "CREATE " (if temp? "TEMPORARY " "") "TABLE "
            (if ifnotexists? "IF NOT EXISTS " "")
            (emit-name name)
            " AS (" (emit-statement rhs) ")")]))

    (define/public (emit-column c)
      (match c
        [(column name type not-null? maybe-default)
         (J (emit-ident name) " " (emit-type type)
            (if not-null? " NOT NULL" "")
            (if maybe-default
                (J " DEFAULT " (emit-scalar-expr maybe-default))
                ""))]))

    (define/public (emit-constraint c)
      (match c
        [(constraint:named name c)
         (J "CONSTRAINT " (emit-ident name) " " (emit-constraint c))]
        [(constraint:primary-key columns)
         (J "PRIMARY KEY (" (emit-ident-commalist columns) ")")]
        [(constraint:unique columns)
         (J "UNIQUE (" (emit-ident-commalist columns) ")")]
        [(constraint:check expr)
         (J "CHECK (" (emit-scalar-expr expr) ")")]
        [(constraint:references columns foreign-table foreign-cols on-delete on-update)
         (define (emit-action which action?)
           (if action?
               (J " ON " which " " (match action?
                                     ['set-null "SET NULL"]
                                     ['set-default "SET DEFAULT"]
                                     ['cascade "CASCADE"]
                                     ['restrict "RESTRICT"]
                                     ['no-action "NO ACTION"]))
               ""))
         (J "FOREIGN KEY (" (emit-ident-commalist columns) ") REFERENCES "
            (emit-name foreign-table)
            (if foreign-cols
                (J "(" (emit-ident-commalist foreign-cols) ")")
                "")
            (emit-action "DELETE" on-delete)
            (emit-action "UPDATE" on-update))]))

    ;; ----------------------------------------
    ;; With

    (define/public (emit-with s)
      (match s
        [(statement:with rec? headers rhss body)
         (J "WITH "
            (if rec? "RECURSIVE " "")
            (J-join (for/list ([h (in-list headers)] [rhs (in-list rhss)])
                      (J (emit-with-header h) " AS (" (if (statement-ast? rhs)
                                                          (emit-statement rhs)
                                                          (emit-table-expr rhs)) ")"))
                    ", ")
            " "
            (emit-statement body))]))

    (define/public (emit-with-header s)
      (match s
        [(cons name #f)
         (emit-ident name)]
        [(cons name columns)
         (J (emit-ident name) "(" (emit-ident-commalist columns) ")")]))

    ;; ----------------------------------------
    ;; Select

    (define/public (emit-select s)
      (match s
        [(statement:select distinct vals from where groupby having ext)
         (J "SELECT "
            (case distinct [(all) "ALL "] [(distinct) "DISTINCT "] [(#f) ""])
            (J-join (map emit-select-item vals) ", ")
            (emit-select-from from)
            (emit-where where)
            (emit-select-groupby groupby)
            (emit-select-having having)
            (emit-select-extension ext))]))

    (define/public (emit-select-item si)
      (match si
        [(select-item:as expr var)
         (J (emit-scalar-expr expr) " AS " (emit-ident var))]
        [(select-item:all)
         "*"]
        [_ (emit-scalar-expr si)]))

    (define/public (emit-select-order so)
      (match so
        [(select:order column asc/desc)
         (J (emit-scalar-expr column)
            (case asc/desc
              [(asc) " ASC"]
              [(desc) " DESC"]
              [(#f) ""]))]))

    (define/public (emit-select-from from)
      (if (pair? from)
          (J " FROM " (J-join (map emit-table-ref from) ", "))
          ""))

    (define/public (emit-where where)
      (if (pair? where)
          (J " WHERE " (J-join (map emit-scalar-expr where) " AND "))
          ""))

    (define/public (emit-select-groupby groupby)
      (if (pair? groupby)
          (J " GROUP BY " (J-join (map emit-name groupby) ", "))
          ""))

    (define/public (emit-select-having having)
      (if (pair? having)
          (J " HAVING " (J-join (map emit-scalar-expr having) " AND "))
          ""))

    (define/public (emit-select-extension ext)
      (match ext
        [(select:extension order limit offset)
         (J (if (pair? order)
                (J " ORDER BY " (J-join (map emit-select-order order) ", "))
                "")
            (if limit
                (J " LIMIT " (emit-scalar-expr limit))
                "")
            (if offset
                (J " OFFSET " (emit-scalar-expr offset))
                ""))]
        [#f ""]))

    ;; ----------------------------------------

    (define/public (emit-insert i)
      (match i
        [(statement:insert table columns source on-conflict)
         (case on-conflict
           [(#f) (J "INSERT INTO " (emit-insert* i))]
           [(ignore)
            (case (send dialect insert/on-conflict-style)
              [(sqlite3) (J "INSERT OR IGNORE INTO " (emit-insert* i))]
              [(postgresql) (J "INSERT INTO " (emit-insert* i) " ON CONFLICT DO NOTHING ")]
              [(mysql)
               ;; MySQL "INSERT IGNORE" is evil; it suppresses errors unrelated to UNIQUE
               ;; constraints. "ON DUPLICATE KEY UPDATE" requires at least one assignment
               ;; so we generate a dummy assignment that should have no effect.
               ;; See https://stackoverflow.com/questions/2366813/on-duplicate-key-ignore
               (unless (pair? columns)
                 (error 'emit-insert "ignore option not supported with empty column list~a"
                        (send dialect error-line)))
               (J "INSERT INTO " (emit-insert* i)
                  " ON DUPLICATE KEY /* DO NOTHING */ UPDATE "
                  (let ([col0 (emit-ident (car columns))])
                    (J col0 " = " col0 " ")))]
              [else (error 'emit-insert "ignore option not supported~a"
                           (send dialect error-line))])])]))

    (define/public (emit-insert* i)
      (match i
        [(statement:insert table columns source _)
         (J (emit-name table)
            (emit-insert-columns columns)
            (emit-table-expr source))]))

    (define/public (emit-insert-columns columns)
      (if columns
          (J " (" (emit-ident-commalist columns) ") ")
          " "))

    ;; ----------------------------------------

    (define/public (emit-update u)
      (match u
        [(statement:update table assign where)
         (J "UPDATE "
            (emit-name table)
            " SET "
            (J-join (map emit-update-assign assign) ", ")
            (emit-where where))]))

    (define/public (emit-update-assign a)
      (match a
        [(update:assign column expr)
         (J (emit-ident column) " = " (emit-scalar-expr expr))]))

    ;; ----------------------------------------

    (define/public (emit-delete d)
      (match d
        [(statement:delete table where)
         (J "DELETE FROM "
            (emit-name table)
            (emit-where where))]))

    ;; ----------------------------------------

    (define/public (emit-table-ref t)
      (match t
        [(table-ref:inject sql)
         sql]
        [(table-ref:name table-name)
         (emit-name table-name)]
        [(table-ref:as (table-ref:name table-name) rangevar)
         (J (emit-name table-name) " AS " (emit-ident rangevar))]
        [(table-ref:as table-expr rangevar)
         (J "(" (emit-table-expr table-expr) ") AS " (emit-ident rangevar))]
        [(? join-table-expr?)
         (emit-join-table-expr t)]
        [_
         ;; (eprintf "WARNING: may be illegal syntax\n")
         (J "(" (emit-table-expr t) ")")]))

    ;; ----------------------------------------

    ;; Emit according to concrete syntax for minimal parenthesization.

    (define/public (emit-table-expr t)
      (match t
        [(table-expr:inject sql)
         sql]
        [(? join-table-expr?)
         (emit-join-table-expr t)]
        [(? nonjoin-table-expr?)
         (emit-nonjoin-table-expr t)]))

    (define/public (emit-join-table-expr t)
      (match t
        [(table-expr:cross-join t1 t2)
         (J (emit-table-ref t1)
            " CROSS JOIN "
            (emit-table-ref t2))]
        [(table-expr:join type t1 t2 on)
         (J (emit-table-ref t1)
            (emit-join-on/part1 on)
            (emit-join-type type)
            (emit-table-ref t2)
            (emit-join-on/part2 on))]))

    (define/public (emit-join-on/part1 on)
      (match on
        [`(natural) " NATURAL"]
        [_ ""]))

    (define/public (emit-join-type type)
      (match type
        ['inner-join " INNER JOIN "]
        ['left-join  " LEFT OUTER JOIN "]
        ['right-join " RIGHT OUTER JOIN "]
        ['full-join  " FULL OUTER JOIN "]
        ['union-join " UNION JOIN "]))

    (define/public (emit-join-on/part2 on)
      (match on
        [`(using ,columns)
         (J " USING (" (emit-ident-commalist columns) ")")]
        [`(on ,condition)
         (J " ON " (emit-scalar-expr condition))]
        [_ ""]))

    ;; ----------------------------------------

    (define/public (emit-nonjoin-table-expr t)
      (match t
        [(table-expr:set-op (and type (or 'union 'except)) t1 t2 opt corr)
         (J (emit-table-expr t1)
            (emit-set-op-parts type opt corr)
            (emit-table-term t2))]
        [_ (emit-nonjoin-table-term t)]))

    (define/public (emit-table-term t)
      (cond [(join-table-expr? t)
             (emit-join-table-expr t)]
            [else
             (emit-nonjoin-table-term t)]))

    (define/public (emit-nonjoin-table-term t)
      (match t
        [(table-expr:set-op (and type 'intersect) t1 t2 opt corr)
         (J (emit-table-term t1)
            (emit-set-op-parts type opt corr)
            (emit-table-primary t2))]
        [_ (emit-nonjoin-table-primary t)]))

    (define/public (emit-table-primary t)
      (cond [(join-table-expr? t)
             (emit-join-table-expr t)]
            [else
             (emit-nonjoin-table-primary t)]))

    (define/public (emit-nonjoin-table-primary t)
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

    (define/public (emit-set-op-parts type opt corr)
      (J (case type
           [(union) " UNION "]
           [(except) " EXCEPT "]
           [(intersect) " INTERSECT "])
         (match opt
           [#t "ALL "]
           [#f ""])
         (match corr
           [`#f ""]
           [`#t "CORRESPONDING "]
           [(list columns ...)
            (J "CORRESPONDING (" (emit-ident-commalist columns) ") ")])))

    ;; ----------------------------------------

    (define/public (emit-scalar-expr e)
      (match e
        [(scalar:inject sql)
         sql]
        [(scalar:app op args)
         (cond [(name-ast? op)
                (apply (fun-op (emit-function-name op)) (map emit-scalar-expr args))]
               [else
                (match (send dialect op-entry op)
                  [(list arity formatter)
                   (unless (arity-includes? arity (length args))
                     (error 'emit-scalar-expr
                            (~a "wrong arity for operator or special function"
                                "\n  operator: ~e\n  expected:  ~s\n  given: ~s~a")
                            op (arity->string arity) (length args) (send dialect error-line)))
                   (apply formatter (map emit-scalar-expr args))]
                  [_
                   (error 'emit-scalar-expr
                          "operator not supported\n  operator: ~e" op)])])]
        [(scalar:case cases else)
         (J "CASE"
            (for/list ([c (in-list cases)]) (emit-case-clause c))
            " ELSE " (emit-scalar-expr else) " END")]
        [(scalar:case-of value cases else)
         (J "CASE " (emit-scalar-expr value)
            (for/list ([c (in-list cases)]) (emit-case-clause c))
            " ELSE " (emit-scalar-expr else) " END")]
        [(scalar:exists te)
         (J "EXISTS (" (emit-table-expr te) ")")]
        [(scalar:in-table e1 e2)
         (J "(" (emit-scalar-expr e1) " IN (" (emit-table-expr e2) "))")]
        [(scalar:in-values e1 es2)
         (J "(" (emit-scalar-expr e1) " IN ("
            (J-join (for/list ([e2 (in-list es2)]) (emit-scalar-expr e2)) ", ")
            "))")]
        [(scalar:some/all op e1 quant e2)
         (define quant-string (case quant [(some) "SOME"] [(all) "ALL"]))
         (unless (send dialect some/all-op? op)
           (error 'emit-scalar-expr "illegal operator for ~a comparison\n  operator: ~e~a"
                  quant-string op (send dialect error-line)))
         (unless (or (table-expr-ast? e2) (send dialect some/all-allow-scalar))
           (error 'emit-scalar-expr
                  "scalar expression not supported in ~a comparison\n  operator: ~e~a"
                  quant-string op (send dialect error-line)))
         (J "(" (emit-scalar-expr e1) " "
            (emit-operator-symbol op) " " quant-string " ("
            (if (table-expr-ast? e2) (emit-table-expr e2) (emit-scalar-expr e2))
            "))")]
        [(scalar:table te)
         (J "(" (emit-table-expr te) ")")]
        [(scalar:placeholder)
         (emit-placeholder)]
        [(? name-ast? e)
         (emit-name e)]
        [(? string?)
         (J "'" (regexp-replace* #rx"'" e "''") "'")]
        [(? exact-integer?)
         (number->string e)]))

    (define/public (emit-placeholder)
      (case (send dialect placeholder-style)
        [(?) "?"]
        [($) (begin0 (format "$~s" (next-dollar-placeholder))
               (next-dollar-placeholder (add1 (next-dollar-placeholder))))]))

    (define/private (emit-case-clause c)
      (J " WHEN " (emit-scalar-expr (car c))
         " THEN " (emit-scalar-expr (cdr c))))

    ;; ----------------------------------------

    (define/public (emit-name n [ctx #f])
      (match n
        [(qname qual id)
         (J (emit-name qual) "." (emit-ident id))]
        [_ (emit-ident n ctx)]))

    (define/public (emit-type type)
      (if (name-ast? type)
          (emit-name type '-type)
          (emit-scalar-expr type)))
    (define/public (emit-function-name n)
      (emit-name n '-function))

    (define/public (emit-ident id [ctx #f])
      (match id
        [(id:quoted (? string? str))
         (send dialect J-quote-id str #f)]
        [(id:normal (? symbol? sym))
         (define str (symbol->string sym))
         (cond [(send dialect reserved-word? str ctx)
                (send dialect J-quote-id str #t)]
               [(send dialect id-no-quote? str)
                str]
               [else (send dialect J-quote-id str #t)])]))

    (define/public (emit-ident-commalist ids)
      (J-join (for/list ([id (in-list ids)]) (emit-ident id)) ", "))

    (define/public (emit-operator-symbol sym)
      (symbol->string sym))
    ))


(define standard-emit-sql (new emit-sql% (dialect standard-dialect)))
(define postgresql-emit-sql (new emit-sql% (dialect postgresql-dialect)))
(define mysql-emit-sql (new emit-sql% (dialect mysql-dialect)))
(define sqlite3-emit-sql (new emit-sql% (dialect sqlite3-dialect)))
