#lang racket/base
(require racket/string
         racket/include
         racket/class
         racket/list
         (rename-in racket/match [match-define defmatch])
         racket/format
         "ast.rkt"
         "jumble.rkt")
(provide (all-defined-out))

;; ----------------------------------------

;; So we can use map with method names
(define-syntax-rule (map f xs) (for/list ([x (in-list xs)]) (f x)))

;; reserved-word-table : (Hash String => (Listof Symbol))
(define reserved-word-table
  (include "keywords.rktd"))

;; reserved-word/dialect? : (U String Symbol) Symbol [(U '-type '-function #f)] -> Boolean
(define (reserved-word/dialect? s dialect [ctx #f])
  (define key (string-downcase (if (symbol? s) (symbol->string s) s)))
  (define vals (hash-ref reserved-word-table key null))
  (and (memq dialect vals) (not (memq ctx vals))))

;; ----------------------------------------

(define dialect<%>
  (interface ()
    placeholder-style   ;; -> (U '? '$)
    error-line          ;; -> String

    ;; Identifiers (already converted to strings)
    reserved-word?      ;; String ctx:(U Symbol #f) -> Boolean
    id-no-quote?        ;; String -> Boolean
    J-quote-id          ;; String Boolean -> J

    ;; Operators/Specials
    op-entry            ;; op:Symbol -> (U (list Arity Formatter) #f)
    some/all-op?        ;; op:Symbol -> Boolean
    some/all-allow-scalar ;; -> Boolean
    special-var         ;; var:Symbol -> (U String #f)

    ;; INSERT on conflict style
    insert/on-conflict-style ;; -> (U 'postgresql 'sqlite3 #f)
    ))

;; Arity is defined in ast.rkt

;; ----------------------------------------

;; A Formatter is (String ... -> String)

(define ((fun-op op-string) . args)
  (J op-string "(" (J-join args ", ") ")"))

(define ((weird-fun-op op-string arg-prefixes) . args)
  (J op-string "("
     (for/list ([prefix (in-list arg-prefixes)] [arg (in-list args)])
       (if prefix (J prefix arg) arg))
     ")"))

(define ((infix-op separator) . args)
  (J "(" (J-join args separator) ")"))

(define ((prefix-op prefix) arg)   (J "(" prefix arg ")"))
(define ((postfix-op postfix) arg) (J "(" arg postfix ")"))

(define ((outfix-op separators) . args)
  (J "(" (car args)
     (for/list ([arg (in-list (cdr args))]
                [separator (in-list separators)])
       (J separator arg))
     ")"))

;; ----------------------------------------

;; An OpEntry is one of
;; - (list Symbol Arity Formatter)
;; - (list Regexp (RxMatch -> (list Arity Formatter)))

(define standard-operator-symbol-rx
  #rx"^(?:=|<|<=|>|>=|<>)$")

(define standard-ops
  `(;; Functions
    [cast         2  ,(weird-fun-op "CAST" '(#f " AS "))]
    [extract      2  ,(weird-fun-op "EXTRACT" '(#f " FROM "))]
    [overlay   (3 4) ,(weird-fun-op "OVERLAY" '(#f " PLACING " " FROM " " FOR "))]
    [position     2  ,(weird-fun-op "POSITION" '(#f " IN "))]
    [substring (2 3) ,(weird-fun-op "SUBSTRING" '(#f "FROM" "FOR"))]
    [trim-leading  2 ,(lambda (arg1 arg2) (J "TRIM(LEADING "  arg1 " FROM " arg2 ")"))]
    [trim-trailing 2 ,(lambda (arg1 arg2) (J "TRIM(TRAILING " arg1 " FROM " arg2 ")"))]
    [trim-both     2 ,(lambda (arg1 arg2) (J "TRIM(BOTH "     arg1 " FROM " arg2 ")"))]
    [count-all     0 ,(lambda () "COUNT(*)")]

    ;; Operators
    [||           #&1 ,(infix-op " || ")] ;; HACK! Note "||" reads as the empty symbol!
    [\|\|         #&1 ,(infix-op " || ")]
    [+            #&1 ,(infix-op " + ")]
    [-            #&2 ,(infix-op " - ")]
    [*            #&1 ,(infix-op " * ")]
    [/            #&1 ,(infix-op " / ")]
    [and          #&1 ,(infix-op " AND ")]
    [or           #&1 ,(infix-op " OR ")]
    [not            1 ,(prefix-op "NOT ")]
    [is-null        1 ,(postfix-op " IS NULL")]
    [is-not-null    1 ,(postfix-op " IS NOT NULL")]
    [is-true        1 ,(postfix-op " IS TRUE")]
    [is-not-true    1 ,(postfix-op " IS NOT TRUE")]
    [is-false       1 ,(postfix-op " IS FALSE")]
    [is-not-false   1 ,(postfix-op " IS NOT FALSE")]
    [is-unknown     1 ,(postfix-op " IS UNKNOWN")]
    [is-not-unknown 1 ,(postfix-op " IS NOT UNKNOWN")]
    [collate        2 ,(infix-op   " COLLATE ")]
    [distinct-from  2 ,(infix-op " DISTINCT FROM ")]
    [between-and    3 ,(outfix-op '(" BETWEEN " " AND "))]
    [like       (2 3) ,(outfix-op '(" LIKE " " ESCAPE "))]
    [ilike      (2 3) ,(outfix-op '(" ILIKE " " ESCAPE "))]
    [similar-to (2 3) ,(outfix-op '(" SIMILAR TO " " ESCAPE "))]
    [,standard-operator-symbol-rx
     ,(lambda (op) (list 2 (infix-op (format " ~a " op))))]

    ;; Field reference
    ;; (.field x)    "x.field"
    [#rx"^[.]([a-zA-Z_][a-zA-Z_0-9]*)$"
     ,(lambda (op field-name) (list 1 (lambda (arg) (J "(" arg ")." field-name))))]
    ;; (.*) = "*", (.* t) = "t.*"
    [.*      (0 1) ,(case-lambda [() "*"] [(arg) (J "(" arg ").*")])]

    ;; Other notations
    [%row    #&2 ,(lambda args (J "(" (J-join args ", ") ")"))]
    ))

(define (lookup-op ops op)
  (let loop ([ops ops])
    (cond [(null? ops) #f]
          [(symbol? (caar ops))
           (cond [(eq? op (caar ops))
                  (cdar ops)]
                 [else (loop (cdr ops))])]
          [(regexp? (caar ops))
           (cond [(regexp-match (caar ops) (symbol->string op))
                  => (lambda (m) (apply (cadar ops) m))]
                 [else (loop (cdr ops))])])))

(define (lookup-op-arity ops op-name)
  (cond [(lookup-op ops op-name) => car]
        [else #f]))

(define (lookup-op-formatter ops op-name)
  (cond [(lookup-op ops op-name) => cadr]
        [else #f]))

;; ----------------------------------------

(define standard-special-vars
  '([NULL                "NULL"]
    [false               "FALSE"]
    [true                "TRUE"]
    [current_date        "CURRENT_DATE"]
    [current_catalog     "CURRENT_CATALOG"]
    [current_role        "CURRENT_ROLE"]
    [current_schema      "CURRENT_SCHEMA"]
    [current_time        "CURRENT_TIME"]
    [current_timestamp   "CURRENT_TIMESTAMP"]
    [current_user        "CURRENT_USER"]))

;; ============================================================

(define standard-dialect%
  (class* object% (dialect<%>)
    (super-new)

    ;; ----------------------------------------

    (define/public (placeholder-style) '?)

    (define/public (error-line) "\n  dialect: SQL:1992")

    ;; ----------------------------------------
    ;; Identifiers

    (define/public (reserved-word? s ctx)
      (reserved-word/dialect? s (get-reserved-key) ctx))

    (define/public (get-reserved-key) 'sql92)

    (define/public (id-no-quote? s)
      (regexp-match? (no-quote-rx) s))

    (define/public (no-quote-rx)
      #rx"^[a-zA-Z][a-zA-Z0-9_]*$")

    (define/public (J-quote-id s fold-case?)
      (define q (id-quote-string))
      (define qq (id-quote-quote-string))
      (J q (string-replace (if fold-case? (id-fold-case s) s) q qq) q))

    (define/public (id-quote-string) "\"")
    (define/public (id-quote-quote-string) "\"\"")
    (define/public (id-fold-case s) (string-upcase s))

    ;; ----------------------------------------
    ;; Operators and Specials

    (define/public (op-entry op)
      (lookup-op (operator-table) op))

    (define/public (operator-table)
      standard-ops)

    (define/public (some/all-op? op)
      (memq op '(= < > <= >= <>)))

    (define/public (some/all-allow-scalar)
      #f)

    (define/public (special-var var)
      (cond [(assq var (special-var-table)) => cdr] [else #f]))

    (define/public (special-var-table)
      standard-special-vars)

    (define/public (insert/on-conflict-style) #f)
    ))

;; ------------------------------------------------------------
;; PostgreSQL

;; Operators: http://www.postgresql.org/docs/current/static/sql-syntax-lexical.html
;; Any string of one or more from {+ - * / < > = ~ ! @ # % ^ & | ` ?}, EXCEPT:
;; - no "--" or "/*" anywhere ==> "/(?![*])", "-(?!-)"
;; - cannot end in {+ -} unless also contains {~ ! @ # % ^ & | ` ?} -- call that (1)
;;   that is, can match + or - if ((1) matched) or (not at end)
(define pg-operator-symbol-rx
  #rx"^(?:([~!@#%^&|`?])|[*<>=]|/(?![*])|[+](?(1)|(?!$))|-(?(1)|(?!$))(?!-))+$")

(define pg-ops-ext
  `([,pg-operator-symbol-rx
     ,(lambda (op _x) (list 2 (infix-op (format " ~a " op))))]
    [row     #&0 ,(fun-op "ROW")]
    [%array  #&1 ,(lambda args (J "ARRAY[" (J-join args ", ") "]"))]
    [%ref    #&2 ,(lambda (array . indexes) (J "(" array ")[" (J-join indexes ", ") "]"))]))

(define pg-ops (append pg-ops-ext standard-ops))

(define postgresql-dialect%
  (class standard-dialect%
    (super-new)

    (define/override (placeholder-style) '$)
    (define/override (error-line) "\n  dialect: PostgreSQL")
    (define/override (get-reserved-key) 'pgsql)
    (define/override (operator-table) pg-ops)

    (define/override (no-quote-rx)
      ;; http://www.postgresql.org/docs/current/static/sql-syntax-lexical.html
      ;; First: alpha or _; Following: letters, underscores, digits, $
      #px"^\\p{L}(?:\\p{L}|[0-9_$])*$")

    (define/override (id-fold-case s)
      ;; testing says only ASCII letters are downcased
      (string-ascii-downcase s))

    (define/override (some/all-op? op)
      ;; No way to tell what operators are comparison ops, so accept all.
      (regexp-match? pg-operator-symbol-rx (symbol->string op)))

    (define/override (some/all-allow-scalar)
      #t)

    (define/override (insert/on-conflict-style) 'postgresql)
    ))

;; string-ascii-downcase : String -> String
;; Downcase only the ASCII letters of s; leave other chars alone.
(define (string-ascii-downcase s)
  (if (for/and ([c (in-string s)]) (< 0 (char->integer c) 128))
      (string-downcase s)
      (let ([s (string-copy s)])
        (for ([c (in-string s)] [i (in-range (string-length s))])
          (when (and (< 0 (char->integer c) 128) (not (eqv? c (char-downcase c))))
            (string-set! s i (char-downcase c))))
        s)))

;; ------------------------------------------------------------
;; MySQL

;; Operators: http://dev.mysql.com/doc/refman/5.7/en/func-op-summary-ref.html
;; Standard ops plus: {&& & ~ | ^ / <=> -> << % != >>)

(define my-operator-symbol-rx
  #rx"^(?:&&|&|~|[\\^]|/|<=>|->|<<|%|!=|>>)$")

(define my-ops-ext
  `([,my-operator-symbol-rx
     ,(lambda (op) (list 2 (infix-op (format " ~a " op))))]))

(define my-ops (append my-ops-ext standard-ops))

(define mysql-dialect%
  (class standard-dialect%
    (super-new)

    (define/override (error-line) "\n  dialect: MySQL")
    (define/override (get-reserved-key) 'mysql)
    (define/override (operator-table) my-ops)

    (define/override (no-quote-rx)
      ;; http://dev.mysql.com/doc/refman/5.7/en/identifiers.html
      ;; FIXME: this isn't quite right
      #px"^\\p{L}(?:\\p{L}|[0-9_$])*$")

    (define/override (id-quote-string) "`")
    (define/override (id-quote-quote-string) "``") ;; FIXME: test
    (define/override (id-fold-case s) s) ;; ... kindof ... whatever

    (define/override (some/all-op? op)
      (or (memq op '(<=> !=)) (super some/all-op? op)))

    (define/override (insert/on-conflict-style) 'mysql)
    ))

;; ------------------------------------------------------------
;; SQLite3

(define sqlite3-ops
  `(;; Functions
    [cast         2  ,(weird-fun-op "CAST" '(#f " AS "))]
    [substring (2 3) ,(fun-op "SUBSTR")]
    [trim-leading  2 ,(fun-op "LTRIM")]
    [trim-trailing 2 ,(fun-op "RTRIM")]
    [trim-both     2 ,(fun-op "TRIM")]
    [count-all     0 ,(lambda () "COUNT(*)")]

    ;; Operators
    [||           #&1 ,(infix-op " || ")] ;; HACK! Note "||" reads as the empty symbol!
    [\|\|         #&1 ,(infix-op " || ")]
    [+            #&1 ,(infix-op " + ")]
    [-            #&2 ,(infix-op " - ")]
    [*            #&1 ,(infix-op " * ")]
    [/            #&1 ,(infix-op " / ")]
    [and          #&1 ,(infix-op " AND ")]
    [or           #&1 ,(infix-op " OR ")]
    [not            1 ,(prefix-op "NOT ")]
    [is-null        1 ,(postfix-op " IS NULL")]
    [is-not-null    1 ,(postfix-op " IS NOT NULL")]
    [collate        2 ,(infix-op   " COLLATE ")]
    [between-and    3 ,(outfix-op '(" BETWEEN " " AND "))]
    [like       (2 3) ,(outfix-op '(" LIKE " " ESCAPE "))]
    [glob       (2 3) ,(outfix-op '(" GLOB " " ESCAPE "))]
    [regexp     (2 3) ,(outfix-op '(" REGEXP " " ESCAPE "))]
    [match      (2 3) ,(outfix-op '(" MATCH " "ESCAPE "))]
    [,standard-operator-symbol-rx
     ,(lambda (op) (list 2 (infix-op (format " ~a " op))))]

    ;; (.*) = "*", (.* t) = "t.*"
    [.*      (0 1) ,(case-lambda [() "*"] [(arg) (J "(" arg ").*")])]
    ;; Other notations
    [%row    #&2 ,(lambda args (J "(" (J-join args ", ") ")"))]
    ))

(define sqlite3-dialect%
  (class standard-dialect%
    (super-new)

    (define/override (error-line) "\n  dialect: SQLite3")
    (define/override (get-reserved-key) 'sqlite)

    (define/override (no-quote-rx)
      ;; FIXME: this is just a guess, really
      #px"^\\p{L}(?:\\p{L}|[0-9_$])*$")

    ;; SQLite does "case-insensitive" (ASCII parts only) comparison of
    ;; all identifiers, including quoted identifiers, so no point in
    ;; case-folding. See also http://www.sqlite.org/faq.html#q18.
    (define/override (id-fold-case s) s)

    (define/override (operator-table) sqlite3-ops)
    (define/override (some/all-op? op) #f) ;; SQLite doesn't support
    (define/override (some/all-allow-scalar) #f)

    (define/override (insert/on-conflict-style) 'sqlite3)
    ))

;; ============================================================

(define standard-dialect (new standard-dialect%))
(define postgresql-dialect (new postgresql-dialect%))
(define mysql-dialect (new mysql-dialect%))
(define sqlite3-dialect (new sqlite3-dialect%))
