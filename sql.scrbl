#lang scribble/manual
@(require scribble/manual
          scribble/basic
          scribble/eval
          (for-label racket
                     racket/contract
                     (only-in db/base query-value)
                     sql))

@title{SQL: A Racket Notation for SQL}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define (lit str) (racketfont str))

@(define the-eval (make-base-eval))
@(the-eval '(require sql))

@defmodule[sql]

@; ============================================================
@section[#:tag "sql-syntax"]{S-expression Syntax for SQL}

This section describes this library's S-expression syntax for (a
subset of) SQL. All literals are recognized symbolically, rather than
by identifier binding, to avoid cluttering the namespace.

Each non-terminal has an S-expression syntax, an AST type, an AST type
predicate, a macro to produce AST values from the S-expression syntax,
and a procedure that converts AST values to SQL strings. The AST type
representations are not considered public; they may change in future
versions of this library.

@; ----------------------------------------
@subsection[#:tag "names"]{SQL Names and Identifiers}

A name is either an unqualified identifier or an identifier qualified
with a name, which depending on its usage might represent a catalog,
schema, table, range variable, etc. A ``regular identifier'' matching
the pattern @racket[#rx"[a-zA-Z][a-zA-Z0-9_]"] may be written as a
single symbol, and a qualified name whose components are all regular
identifier can be written as a single symbol with its components
separated by dot characters.

A name or identifier written as a symbol is subject to case-folding,
whereas one written as a string is not case-folded but interpreted
literally. Because case-folding behavior is system-dependent, it is
wisest to either always quote a given name or never quote it.

@;{ mention reserved words }

@racketgrammar*[

[name symbol
      ident
      (@#,lit{Name:} name ...+)]

[ident symbol
       (@#,lit{Ident:} string)
       (@#,lit{Ident:} symbol)]

]

The following examples are all equivalent to the SQL qualified name
@tt{table.name}, which by case-folding is also equivalent to
@tt{TABLE.NAME} and @tt{taBLE.naME}:

@racketblock[
table.name
taBLE.naME
(Name: table NAME)
(Name: (Ident: table) (Ident: name))
]

The following example is equivalent to the SQL qualified name
@tt{"Table"."Name"}:

@racketblock[
(Name: (Ident: "Table") (Ident: "Name"))
]

@deftogether[[
@defform[(SQL:Name name)]
@defproc[(name? [v any/c]) boolean?]
@defproc[(name->string [ast name?] [dialect any/c (current-sql-dialect)]) string?]
]]{

Constructor macro, predicate, and code generator, respectively, for
@svar[name].

@examples[#:eval the-eval
(name->string (SQL:Name table.name))
(name->string (SQL:Name (Name: table NAME)))
(name->string (SQL:Name (Name: (Ident: "taBLE") (Ident: "naME"))))
]
}

@deftogether[[
@defform[(SQL:Ident ident)]
@defproc[(ident? [v any/c]) boolean?]
@defproc[(ident->string [ast ident?] [dialect any/c (current-sql-dialect)]) string?]
]]{

Constructor macro, predicate, and code generator, respectively, for
@svar[ident].
}

@; ----------------------------------------
@subsection[#:tag "scalar-exprs"]{SQL Scalar Expressions}

A scalar expression is either a name, a literal integer or string
value, or an application of some function or operator.

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

In an abuse of syntax, types can also be written as ``scalar
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

@deftogether[[
@defform[(SQL:ScalarExpr scalar-expr)]
@defproc[(scalar-expr? [v any/c]) boolean?]
@defproc[(scalar-expr->string [ast scalar-expr?] [dialect any/c (current-sql-dialect)]) string?]
]]{

Constructor macro, predicate, and code generator, respectively, for
@svar[scalar-expr].

@examples[#:eval the-eval
(scalar-expr->string (SQL:ScalarExpr table.column))
(scalar-expr->string (SQL:ScalarExpr 42))
(scalar-expr->string (SQL:ScalarExpr "Salutations"))
(scalar-expr->string (SQL:ScalarExpr "a 'tricky' string"))
(scalar-expr->string (SQL:ScalarExpr (log (- 1 p))))
(scalar-expr->string (SQL:ScalarExpr (and (> x 10) (< x 55))))
(scalar-expr->string (SQL:ScalarExpr (coalesce x y z)))
(scalar-expr->string (SQL:ScalarExpr (cast "2015-03-15" DATE)))
(scalar-expr->string (SQL:ScalarExpr (is-null table.column)))
(scalar-expr->string (SQL:ScalarExpr (like ph_num "555-____")))
(scalar-expr->string (SQL:ScalarExpr (extract YEAR dob)))
(scalar-expr->string (SQL:ScalarExpr (string-append last ", " first)))
]
}

@; ----------------------------------------
@subsection[#:tag "table-exprs"]{SQL Table References and Expressions}

A table reference is either a reference to a defined table (or view)
or a computed table with a name or named components. A table
expression can be formed using join and set operations.

@racketgrammar*[

[table-ref
 table-name
 (@#,lit{as} table-name range-var-ident)
 (@#,lit{as} table-expr range-var-ident)
 table-expr]

]

Note: in the final variant of @svar[table-ref], the @svar[table-expr]
must be a join table expression, specifically.

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


@deftogether[[
@defform[(SQL:TableRef table-ref)]
@defproc[(table-ref? [v any/c]) boolean?]
@defproc[(table-ref->string [ast table-ref?] [dialect any/c (current-sql-dialect)]) string?]
]]{

Constructor macro, predicate, and code generator, respectively, for
@svar[table-ref].

@examples[#:eval the-eval
(table-ref->string (SQL:TableRef supplier))
(table-ref->string (SQL:TableRef (as supplier s)))
(table-ref->string (SQL:TableRef (inner-join supplier part #:using (supply_id))))
]
}

@deftogether[[
@defform[(SQL:TableExpr table-expr)]
@defproc[(table-expr? [v any/c]) boolean?]
@defproc[(table-expr->string [ast table-expr?] [dialect any/c (current-sql-dialect)]) string?]
]]{

Constructor macro, predicate, and code generator, respectively, for
@svar[table-expr].
}

@; ----------------------------------------
@subsection[#:tag "statements"]{SQL Statements}

A statement is one of the four standard DML statements.

@racketgrammar*[
[statement select-statement
           insert-statement
           update-statement
           delete-statement]
]

@bold{Select}

@racketgrammar*[

[select-statement (@#,lit{select} select-item ... select-clause ...)
                  (@#,lit{select} select-clause ...)]

[select-clause (code:line #:values select-item ...)
               (code:line #:from table-ref ...)
               (code:line #:where condition-scalar-expr ...)
               (code:line #:group-by column-ident ...)
               (code:line #:having condition-scalar-expr ...)
               (code:line #:order-by order-item ...)
               (code:line #:limit scalar-expr)
               (code:line #:offset scalar-expr)]

[select-item scalar-expr
             (@#,lit{as} scalar-expr ident)]

[order-item (code:line scalar-expr #:asc)
            (code:line scalar-expr #:desc)
            (code:line scalar-expr)]

]

A @svar[select-statement] can contain each kind of
@svar[select-clause] at most once. The clauses can occur in any
order. If the first form of @svar[select-statement] is used (that is,
with the initial @svar[select-item]s), then the @racket[#:values]
clause may not also be used.


@bold{Insert}

@racketgrammar*[

[insert-statement (@#,lit{insert} #:into table-name assign-clause)
                  (@#,lit{insert} #:into table-name maybe-columns
                                  #:from table-expr)]

[assign-clause (code:line #:set [column-ident scalar-expr] ...)]

[maybe-columns (code:line)
               (code:line #:columns (column-ident ...))]

]

@bold{Update}

@racketgrammar*[

[update-statement (@#,lit{update} table-name assign-clause maybe-where)]

[assign-clause (code:line #:set [column-ident scalar-expr] ...)]

[maybe-where (code:line)
             (code:line #:where condition-scalar-expr ...)]

]

@bold{Delete}

@racketgrammar*[

[delete-statement (@#,lit{delete} #:from table-name maybe-where)]

[maybe-where (code:line)
             (code:line #:where condition-scalar-expr ...)]

]

@deftogether[[
@defform[(SQL:Statement statement)]
@defproc[(statement? [v any/c]) boolean?]
@defproc[(statement->string [ast statement?] [dialect any/c (current-sql-dialect)]) string?]
]]{

Constructor macro, predicate, and code generator for @svar[statement].

@examples[#:eval the-eval
(statement->string
 (SQL:Statement (select a b c #:from table #:where (> a 10))))
(statement->string
 (SQL:Statement (insert #:into table #:set [a 1] [b 2] [c 3])))
]
}


@; ============================================================
@section[#:tag "statement-forms"]{Statement Forms}

The macros in this section create statement values suitable for
passing to the query functions of the @racketmodname[db]
library. These statement values are different from @svar[statement]
ASTs.

Note: the printing of a statement value is controlled by
@racket[(current-sql-dialect)], but the code it generates when passed
to a query function is determined by the dialect of the connection the
query is performed on.

@deftogether[[
@defform*[[(select select-item ... select-clause ...)
           (select select-clause ...)]]

@defform*[[(insert #:into table-name assign-clause)
           (insert #:into table-name maybe-columns #:from table-expr)]]

@defform[(update table-name assign-clause maybe-where)]

@defform[(delete #:from table-name maybe-where)]
]]{

Produces a statement value that can be passed to a @racketmodname[db]
query function. The syntax of the macros corresponds to the syntax of
the @svar[select-statement], @svar[insert-statement],
@svar[update-statement], and @svar[delete-statement] nonterminals,
respectively, except that the macro name is recognized by its
identifier binding rather than symbolically.

@examples[#:eval the-eval
(select a b c #:from table #:where (> a 10))
(insert #:into table #:set [a 1] [b 2] [c 3])
(insert #:into table
        #:from (select a b c 
                       #:from other_table
                       #:where (is-not-null d)))
]
}

@defproc[(sql-statement? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a statement value returned by one
of the forms in this section such as @racket[select], @racket[#f]
otherwise.
}

@; ============================================================
@section[#:tag "unquote"]{Placeholders and Unquote}

There are two additional variants of @svar[scalar-expr] that enable
the construction of parameterized queries. The first is a placeholder,
written @lit{?} (regardless of the notation used by the database the
query is to be sent to). The second is the @lit{unquote} form, which
is equivalent to inserting a placeholder and also providing the
expression as a query parameter.

@racketgrammar*[

[scalar-expr ....
             ?
             (@#,lit{unquote} racket-expr)]

]

Note: Due to limitations in the @racketmodname[db] library,
@lit{unquote} parameters and ordinary placeholders cannot be mixed in
the same statement.

@examples[#:eval the-eval
(select a #:from table #:where (= b ?))
]

The resulting statement can be used with parameters thus:

@racketblock[
(query-value c (select a #:from table #:where (= b ?)) 10)
]

Using the @lit{unquote} form eliminates the need to keep track of
positional parameters; instead, the parameter value is written as a
Racket expression within the statement. It is automatically translated
to SQL code containing placeholders.

@examples[#:eval the-eval
(define b-param 10)
(select a #:from table #:where (= b ,b-param))
]

The resulting statement must be called without additional parameters:

@racketblock[
(query-value c (select a #:from table #:where (= b ,b-param)))
]

Note that placeholder syntax varies between SQL dialects. We can see
the code a statement produces for a specific dialect by setting the
@racket[current-sql-dialect] parameter:

@interaction[#:eval the-eval
(parameterize ((current-sql-dialect 'postgresql))
  (print (select a #:from table #:where (= b ,b-param))))
]

@; ============================================================
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
