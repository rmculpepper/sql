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

@racketblock[
table.name
(string-append lastname ", " firstname)
(and (> x 5) (< x 20))
]

@;{ FIXME: need table of functions and operator aliases }

As an abuse of syntax, types are also written as ``scalar
expressions'', for example in @tt{CAST} expressions:

@racketblock[
(cast "2015-03-15" "date") (code:comment "equivalent to CAST('2015-03-15' AS DATE)")
(cast "123" (numeric 5 0)) (code:comment "equivalent to CAST('123' AS NUMERIC(5, 0))")
]

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


@defform*[[(select select-item ... select-clause ...)
           (select select-clause ...)]
          #:grammar
          ([select-clause select-items-clause
                          select-from-clause
                          select-where-clause
                          select-group-by-clause
                          select-having-clause
                          select-order-clause
                          select-limit-clause]
           [select-values-clause
            (code:line #:values select-item ...)]
           [select-from-clause
            (code:line #:from table-reference ...)]
           [select-where-clause
            (code:line #:where condition-scalar-expr ...)]
           [select-group-by-clause
            (code:line #:group-by column-ident ...)]
           [select-having-clause
            (code:line #:having condition-scalar-expr ...)]
           [select-order-clause
            (code:line #:order-by select-order-item ...)]
           [select-limit-clause
            (code:line #:limit scalar-expr)]
           [select-offset-clause
            (code:line #:offset scalar-expr)]
           [select-item scalar-expr
                        (@#,lit{as} scalar-expr ident)]
           [select-order-item
            (code:line scalar-expr #:asc)
            (code:line scalar-expr #:desc)
            (code:line scalar-expr)])]{

}

@defform[(insert insert-target-clause insert-source-clause)
         #:grammar
         ([insert-target-clause
           (code:line #:into table-name)
           (code:line #:into table-name (column-ident ...))]
          [insert-source-clause
           (code:line #:values sclar-expr ...)
           (code:line #:from table-expr)])]{

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
