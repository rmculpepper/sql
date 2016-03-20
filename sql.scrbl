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

This section describes this library's S-expression syntax for a subset
of SQL. The SQL support is organized by nonterminals (eg
@svar[statement], @svar[table-ref], and @svar[scalar-expr]); the
grammar handled by this library is adapted from @emph{A Guide to the
SQL Standard, 4th ed@._} by C@._ J@._ Date and Hugh Darwen. Each
non-terminal has the following:
@itemlist[
@item{an S-expression syntax,}
@item{an AST type predicate,}
@item{a quasiquotation macro to produce AST values from the
S-expression syntax, and}
@item{a procedure that converts AST values to SQL strings.}
]
All literals are recognized symbolically, rather than by identifier
binding, to avoid cluttering the namespace. The AST type
representations are not considered public; they may change in future
versions of this library.


@; ----------------------------------------
@subsection[#:tag "names"]{SQL Names and Identifiers}

A name is either an unqualified identifier or an identifier qualified
with a name, which depending on its usage might represent a catalog,
schema, table, range variable, etc. A ``regular identifier'' matching
the pattern @racket[#rx"[a-zA-Z][a-zA-Z0-9_]*"] may be written as a
single symbol, and a qualified name whose components are all regular
identifier can be written as a single symbol with its components
separated by dot characters.

A name or identifier written as a symbol is subject to case-folding,
whereas one written as a string is not case-folded but interpreted
literally. Because case-folding behavior is system-dependent, it is
wisest to either always quote a given name or never quote it. An
identifier written as @racket[(Ident: _string)] is not case-folded; it
is generated in quoted form.

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
(Name: (Ident: table) (Ident: NAME))
]

The following example is equivalent to the SQL qualified name
@tt{"Table"."Name"}:

@racketblock[
(Name: (Ident: "Table") (Ident: "Name"))
]

@deftogether[[
@defform[(name-qq name)]
@defproc[(name-ast? [v any/c]) boolean?]
@defproc[(name-ast->string [ast name-ast?]
                           [dialect any/c (current-sql-dialect)])
         string?]
]]{

Quasiquotation macro, predicate, and code generator, respectively, for
@svar[name].

@examples[#:eval the-eval
(name-ast->string (name-qq table.name))
(name-ast->string (name-qq (Name: table NAME)))
(name-ast->string (name-qq (Name: (Ident: "taBLE") (Ident: "naME"))))
]
}

@deftogether[[
@defform[(ident-qq ident)]
@defproc[(ident-ast? [v any/c]) boolean?]
@defproc[(ident-ast->string [ast ident-ast?]
                            [dialect any/c (current-sql-dialect)])
         string?]
]]{

Quasiquotation macro, predicate, and code generator, respectively, for
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
             (operator-id scalar-expr ...)
             (name scalar-expr ...)
             (@#,lit{exists} table-expr)
             (@#,lit{in} scalar-expr table-expr)
             (@#,lit{some} scalar-expr operator-symbol table-expr)
             (@#,lit{all} scalar-expr operator-symbol table-expr)
             (@#,lit{case} [scalar-expr scalar-expr] ...)
             (@#,lit{case} [scalar-expr scalar-expr] ... [@#,lit{else} scalar-expr])
             (@#,lit{case} #:of scalar-expr [scalar-expr scalar-expr] ...)
             (@#,lit{case} #:of scalar-expr
               [scalar-expr scalar-expr] ... [@#,lit{else} scalar-expr])
             table-expr]
]

A @tt{CASE} expression:
@racketblock[
(case [(= x 0) "zero"] [else "no"])  (code:comment "CASE WHEN x = 0 THEN 'zero' ELSE 'no' END")
(case #:of x [0 "zero"] [else "no"]) (code:comment "CASE x WHEN 0 THEN 'zero' ELSE 'no' END")
]

The following @svar[operator-id]s are handled specially:

@itemlist[

@item{The @tt{CAST} and @tt{EXTRACT} special forms are written as
normal two-argument functions:

@racketblock[
(cast "2015-03-15" DATE)      (code:comment "CAST('2015-03-15' AS DATE)")
(cast "123" (NUMERIC 5 0))    (code:comment "CAST('123' AS NUMERIC(5, 0))")
(extract YEAR dob)            (code:comment "EXTRACT(YEAR FROM dob)")
]

Note that as above, types and fields are written as ``scalar
expressions'', in a mild abuse of syntax.
}

@item{The @tt{OVERLAY}, @tt{POSITION}, and @tt{SUBSTRING} functions
are written as normal functions:

@racketblock[
(overlay "abc" "z" 2 1)       (code:comment "OVERLAY('abc' PLACING 'z' FROM 2 FOR 1)")
(position "c" "abc")          (code:comment "POSITION('c' IN 'abc)")
(substring "abc" 2 1)         (code:comment "SUBSTRING('abc' FROM 2 FOR 1)")
]}

@item{The @tt{TRIM} function is written using one of the following variants:

@racketblock[
(trim-leading "z" "zzabc")  (code:comment "TRIM(LEADING 'z' FROM 'zzabc')")
(trim-trailing "z" "abczz") (code:comment "TRIM(TRAILING 'z' FROM 'abczz')")
(trim-both "z" "zzabczz")   (code:comment "TRIM(BOTH 'z' FROM 'zzabczz')")
]}

@item{The syntax @tt{COUNT(*)} can be written as follows:

@racketblock[
(count-all)                   (code:comment "COUNT(*)")
]}

@item{The @tt{+}, @tt{-}, @tt{*}, and @tt{/} operators are chaining
infix binary operators written as variadic functions:

@racketblock[
(+ 1 2 3 4)                   (code:comment "1 + 2 + 3 + 4")
]}

@item{The SQL chaining infix binary operator @tt{||} can be written as
@racket[\|\|] or as @racket[||]; the latter reads as the empty symbol.

@racketblock[
(|| last ", " first)          (code:comment "last || ', ' || first")
]}

@item{Any identifier consisting of only characters in
@litchar["~!@#%^&*-_=+|<>?/"] is considered a non-chaining infix
binary operator:

@racketblock[
(< x y)                       (code:comment "x < y")
(%#! 1 2)                     (code:comment "1 %#! 2")
]}

@item{The following operators are written like function calls:

@racketblock[
(is-null x)                   (code:comment "x IS NULL")
(is-not-null x)               (code:comment "x IS NOT NULL")
(is-true x)                   (code:comment "x IS TRUE")
(is-not-true x)               (code:comment "x IS NOT TRUE")
(is-false x)                  (code:comment "x IS FALSE")
(is-not-false x)              (code:comment "x IS NOT FALSE")
(is-unknown x)                (code:comment "x IS UNKNOWN")
(is-not-unknown x)            (code:comment "x IS NOT UNKNOWN")
(collate x utf8)              (code:comment "x COLLATE utf8")
(between-and 5 1 10)          (code:comment "5 BETWEEN 1 AND 10")
(not-between-and 0 1 10)      (code:comment "0 BETWEEN 1 AND 10")
(distinct-from x y)           (code:comment "x DISTINCT FROM y")
(not-distinct-from x y)       (code:comment "x NOT DISTINCT FROM y")
(like "abc" "a%")             (code:comment "'abc' LIKE 'a%'")
(not-like "abc" "z%")         (code:comment "'abc' LIKE 'z%'")
(ilike "aBC" "ab_")           (code:comment "'aBC' ILIKE 'ab_'")
(not-ilike "aBC" "zb_")       (code:comment "'aBC' ILIKE 'zb_'")
(similar-to "abc" "(a|z)%")   (code:comment "'abc' SIMILAR TO '(a|z)%'")
(not-similar-to "" "(a|z)%")  (code:comment "'' NOT SIMILAR TO '(a|z)%'")
]}

@item{Field selection is written as a regular identifier (or @tt{*})
prefixed by a dot.

@racketblock[
(.city state)                 (code:comment "(state).city")
(.* table1)                   (code:comment "(table1).*")
(.*)                          (code:comment "*")
]}

@item{Other names are treated as ordinary functions; no arity checking
is done.

@racketblock[
(coalesce x y z)              (code:comment "coalesce(x, y, z)")
]}

]

@deftogether[[
@defform[(scalar-expr-qq scalar-expr)]
@defproc[(scalar-expr-ast? [v any/c]) boolean?]
@defproc[(scalar-expr-ast->string [ast scalar-expr-ast?]
                                  [dialect any/c
                                  (current-sql-dialect)])
         string?]
]]{

Quasiquotation macro, predicate, and code generator, respectively, for
@svar[scalar-expr].

@examples[#:eval the-eval
(scalar-expr-ast->string (scalar-expr-qq table.column))
(scalar-expr-ast->string (scalar-expr-qq 42))
(scalar-expr-ast->string (scalar-expr-qq "Salutations"))
(scalar-expr-ast->string (scalar-expr-qq "a 'tricky' string"))
(scalar-expr-ast->string (scalar-expr-qq (log (- 1 p))))
(scalar-expr-ast->string (scalar-expr-qq (and (> x 10) (< x 55))))
(scalar-expr-ast->string (scalar-expr-qq (coalesce x y z)))
(scalar-expr-ast->string (scalar-expr-qq (cast "2015-03-15" DATE)))
(scalar-expr-ast->string (scalar-expr-qq (extract YEAR dob)))
(scalar-expr-ast->string (scalar-expr-qq (is-null table.column)))
(scalar-expr-ast->string (scalar-expr-qq (like ph_num "555-____")))
(scalar-expr-ast->string (scalar-expr-qq (|| last ", " first)))
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
 (code:line #:using column-ident ...)
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
 (code:line #:corresponding-by column-ident ...)]

]


@deftogether[[
@defform[(table-ref-qq table-ref)]
@defproc[(table-ref-ast? [v any/c]) boolean?]
@defproc[(table-ref-ast->string [ast table-ref-ast?]
                                [dialect any/c (current-sql-dialect)])
         string?]
]]{

Quasiquotation macro, predicate, and code generator, respectively, for
@svar[table-ref].

@examples[#:eval the-eval
(table-ref-ast->string (table-ref-qq supplier))
(table-ref-ast->string (table-ref-qq (as supplier s)))
(table-ref-ast->string (table-ref-qq (inner-join supplier part #:using supply_id)))
]
}

@deftogether[[
@defform[(table-expr-qq table-expr)]
@defproc[(table-expr-ast? [v any/c]) boolean?]
@defproc[(table-expr-ast->string [ast table-expr-ast?] 
                                 [dialect any/c
                                 (current-sql-dialect)]) 
         string?]
]]{

Quasiquotation macro, predicate, and code generator, respectively, for
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
               (code:line #:columns column-ident ...)]

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
@defform[(statement-qq statement)]
@defproc[(statement-ast? [v any/c]) boolean?]
@defproc[(statement-ast->string [ast statement-ast?] 
                                [dialect any/c (current-sql-dialect)]) 
         string?]
]]{

Constructor macro, predicate, and code generator for @svar[statement].

@examples[#:eval the-eval
(statement-ast->string
 (statement-qq (select a b c #:from table #:where (> a 10))))
(statement-ast->string
 (statement-qq (insert #:into table #:set [a 1] [b 2] [c 3])))
]
}


@; ----------------------------------------
@subsection[#:tag "dialect"]{SQL Dialect}

@defparam[current-sql-dialect
          dialect
          (or/c symbol? dbsystem? connection?)]{

Controls the default dialect used when converting SQL ASTs to strings
using functions such as @racket[statement-ast->string].

This parameter generally @bold{does not} affect statement
(@racket[sql-statement?]) values used with connection query methods;
generation of SQL code for a query method automatically uses the
dialect associated with the connection the query is performed on.
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
