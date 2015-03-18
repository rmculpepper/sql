#lang scribble/manual
@(require scribble/manual
          scribble/basic
          (for-label racket
                     racket/contract
                     sql))

@title{SQL: A Racket Notation for SQL}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define (lit str) (racketfont str))

@defmodule[sql]

@section[#:tag "common"]{SQL Common Non-terminals}

The following non-terminals are used by multiple SQL forms. All
literals are recognized symbolically, rather than by identifier
binding, to avoid cluttering the namespace.

@bold{Names and Identifiers} --- A name is either an unqualified
identifier or an identifier qualified with a name, which depending on
its usage might represent a catalog, schema, table, range variable,
etc. A ``regular identifier'' matching the pattern
@racket[#rx"[a-zA-Z][a-zA-Z0-9_]"] may be written as a single symbol,
and a qualified name whose components are all regular identifier can
be written as a single symbol with its components separated by dot
characters. 

A name or identifier written as a symbol is subject to case-folding,
whereas one written as a string is not case-folded but interpreted
literally. Because case-folding behavior is system-dependent, it is
wisest to either always quote a given name or never quote it.

@;{ mention reserved words }

@racketgrammar*[

[name symbol
      ident
      (@#,lit{qname} name ...+)]

[ident symbol
       (@#,lit{ident} string)
       (@#,lit{ident} symbol)]

]

The following examples are all equivalent to the SQL qualified name
@tt{table.name}, which by case-folding is also equivalent to
@tt{TABLE.NAME} and @tt{taBLE.naME}:

@racketblock[
table.name
taBLE.naME
(qname table NAME)
(qname (ident table) (ident name))
]

The following example is equivalent to the SQL qualified name
@tt{"Table"."Name"}:

@racketblock[
(qname (ident "Table") (ident "Name"))
]

@bold{Scalar Expressions} A scalar expression is either a name, a
literal integer or string value, or an application of some function or
operator.

@racketgrammar*[
[scalar-expr name
             exact-integer
             string
             (name scalar-expr ...)
             (operator-id scalar-expr ...)]
]

The following are examples of scalar expressions:

@racketblock[
table.column
42
"Salutations"
(log (- 1 p))
(and (> x 10) (< x 55))
(coalesce x y z)
]

As an abuse of syntax, types are also written as ``scalar
expressions'', for example in @tt{CAST} expressions:

@racketblock[
(cast "2015-03-15" DATE)    (code:comment "CAST('2015-03-15' AS DATE)")
(cast "123" (NUMERIC 5 0))  (code:comment "CAST('123' AS NUMERIC(5, 0))")
]

Aside from @tt{CAST}, a few other standard SQL functions using
nonstandard function syntax are supported:

@racketblock[
(is-null table.column)      (code:comment "table.column IS NULL")
(like ph_num "555-____")    (code:comment "ph_num LIKE '555-____'")
(extract YEAR dob)          (code:comment "EXTRACT(YEAR FROM dob)")
]

And @lit{string-append} is provided as an alias for @tt{||}, the SQL
concatenation operator, which reads as the empty symbol.

@racketblock[
(string-append last ", " first) (code:comment "last || ', ' || first")
]

Any symbol consisting of only the following characters is considered a
binary operator by default: @litchar["~!@#$%^&*-_=+|<>?/"].

@;{ FIXME: need table of functions and operator aliases }


@bold{Table References}

@racketgrammar*[

[table-reference
 table-name
 (@#,lit{as} table-name range-var-ident)
 (@#,lit{as} table-expr range-var-ident)
 table-expr]

]

@bold{Table Expressions} 

@racketgrammar*[

[table-expr
 (@#,lit{cross-join} table-ref table-ref)
 (join-op table-ref table-ref join-condition)
 (set-op table-ref table-ref
         maybe-all correspond-clause)
 (@#,lit{values} scalar-expr ...)
 (@#,lit{values*} (scalar-expr ...) ...)
 select-statement]

[join-op @#,lit{inner-join}
         @#,lit{left-join}
         @#,lit{right-join}
         @#,lit{full-join}
         @#,lit{union-join}]

[join-condition
 (code:line #:natural)
 (code:line #:using (column-ident ...))
 (code:line #:on scalar-expr)]

[set-op @#,lit{union}
        @#,lit{except}
        @#,lit{intersect}]

[maybe-all
 (code:line)
 (code:line #:all)]

[correspond-clause
 (code:line)
 (code:line #:corresponding)
 (code:line #:corresponding-by (column-ident ...))]

]

@; ============================================================

@section[#:tag "statements"]{Statements}

@defform*[[(select select-item ... select-clause ...)
           (select select-clause ...)]
          #:grammar
          ([select-clause
            (code:line #:values select-item ...)
            (code:line #:from table-reference ...)
            (code:line #:where condition-scalar-expr ...)
            (code:line #:group-by column-ident ...)
            (code:line #:having condition-scalar-expr ...)
            (code:line #:order-by select-order-item ...)
            (code:line #:limit scalar-expr)
            (code:line #:offset scalar-expr)]
           [select-item scalar-expr
                        (@#,lit{as} scalar-expr ident)]
           [select-order-item
            (code:line scalar-expr #:asc)
            (code:line scalar-expr #:desc)
            (code:line scalar-expr)])]{

}

@defform*[[(insert #:into table-name insert-assign-clause)
           (insert #:into table-name maybe-columns-clause #:from table-expr)]
          #:grammar
          ([insert-assign-clause
            (code:line #:set [column-ident scalar-expr] ...)]
           [maybe-columns-clause
            (code:line)
            (code:line #:columns (column-ident ...))])]{

}

@defform[(update table-name update-assign-clause maybe-where-clause)
         #:grammar
         ([update-assign-clause
           (code:line #:set [column-ident scalar-expr] ...)]
          [maybe-where-clause
           (code:line)
           (code:line #:where condition-scalar-expr ...)])]{

}

@defform[(delete delete-from-clause maybe-where-clause)
         #:grammar
         ([delete-from-clause
           (code:line #:from table-name)]
          [maybe-where-clause
           (code:line)
           (code:line #:where condition-scalar-expr ...)])]{

}

@; ============================================================

@section[#:tag "implicit"]{Implicit Placeholders}

An @lit{unquote} form can be used in any scalar expression context. It
is equivalent to inserting a placeholder and providing the expression
as a query parameter.

Note: Due to limitations in the underlying database library,
@lit{unquote} parameters and ordinary placeholders cannot be mixed in
the same statement.

@racketgrammar*[

[scalar-expr ....
             (@#,lit{unquote} racket-expr)]

]

Example:

@racketblock[
(select x #:from t #:where (= y ,y-param))
]

@section[#:tag "escapes"]{Dynamic Statement Composition and SQL Injection}

@racketgrammar*[

[scalar-expr ....
             (@#,lit{ScalarExpr:AST} (@#,lit{unquote} ast-racket-expr))
             (@#,lit{ScalarExpr:INJECT} (@#,lit{unquote} string-racket-expr))]

[table-expr ....
             (@#,lit{TableExpr:AST} (@#,lit{unquote} ast-racket-expr))
             (@#,lit{TableExpr:INJECT} (@#,lit{unquote} string-racket-expr))]

[table-ref ....
           (@#,lit{TableRef:AST} (@#,lit{unquote} ast-racket-expr))
           (@#,lit{TableRef:INJECT} (@#,lit{unquote} string-racket-expr))]
]
