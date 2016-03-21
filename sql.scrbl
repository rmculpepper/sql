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
@item{an AST type predicate, and}
@item{a quasiquotation macro to produce AST values from the
S-expression syntax.}
]
All literals are recognized symbolically, rather than by identifier
binding, to avoid cluttering the namespace. The AST type
representations are not considered public; they are likely to change
in future versions of this library.

@defproc[(sql-ast->string [ast (or/c name-ast? scalar-expr-ast? table-expr-ast?
                                     table-ref-ast? statement-ast?)]
                          [dialect (or/c symbol? #f) (current-sql-dialect)])
         string?]{

Produces SQL code as a string for the given AST to a string according
to the rules of @racket[dialect]. Examples are given throughout the
following sections.
}

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
@tt{mytable.mycolumn}, which by case-folding is also equivalent to
@tt{MYTABLE.MYCOLUMN} and @tt{MyTable.MyColumn}:

@racketblock[
mytable.mycolumn
MyTable.MyColumn
(Name: mytable MYCOLUMN)
(Name: (Ident: mytable) (Ident: MYNAME))
]

The following example is equivalent to the SQL qualified name
@tt{"MyTable"."MyName"}:

@racketblock[
(Name: (Ident: "MyTable") (Ident: "MyName"))
]

@deftogether[[
@defform[(name-qq name)]
@defproc[(name-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for @svar[name].

@examples[#:eval the-eval
(sql-ast->string (name-qq mytable.mycolumn))
(sql-ast->string (name-qq (Name: mytable MYCOLUMN)))
(sql-ast->string (name-qq (Name: (Ident: "MyTable") (Ident: "MyColumn"))))
]

Reserved words are automatically quoted (after case-folding, if
necessary):
@interaction[#:eval the-eval
(sql-ast->string (name-qq table.mycolumn))
(sql-ast->string (name-qq select.insert))
(sql-ast->string (name-qq (Name: (Ident: select) (Ident: insert))))
]}

@deftogether[[
@defform[(ident-qq ident)]
@defproc[(ident-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for @svar[ident].
}

@; ----------------------------------------
@subsection[#:tag "scalar-exprs"]{SQL Scalar Expressions}

A scalar expression is either a name, a literal integer or string
value, or an application of some function or operator.

@racketgrammar*[
[scalar-expr name
             exact-integer
             string
             (@#,lit{exists} table-expr)
             (@#,lit{in} scalar-expr #:from table-expr)
             (@#,lit{in} scalar-expr #:values scalar-expr ...)
             (@#,lit{case} [scalar-expr scalar-expr] ... maybe-else)
             (@#,lit{case} #:of scalar-expr [scalar-expr scalar-expr] ... maybe-else)
             (compare-operator scalar-expr #:some table-expr)
             (compare-operator scalar-expr #:all table-expr)
             (operator/special scalar-expr ...)
             (name scalar-expr ...)
             table-expr]
]

@specsubform[(@#,lit{exists} table-expr)]{

Produces an @tt{EXISTS} expression:

@racketblock[
(exists (select 1 #:from t))        (code:comment "EXISTS (SELECT 1 FROM t)")
]}

@specsubform[(code:line
             (@#,lit{in} scalar-expr #:from table-expr)
             (@#,lit{in} scalar-expr #:values scalar-expr ...))]{

There are two forms of @tt{IN} expression, one for table expressions
and one for lists of scalar expressions:

@racketblock[
(in x #:from (select y #:from ys))  (code:comment "x IN (SELECT y FROM ys)")
(in x #:values 1 2 3)               (code:comment "x IN (1, 2, 3)")
]}

@specsubform[(code:line
             (@#,lit{case} [scalar-expr scalar-expr] ... maybe-else)
             (@#,lit{case} #:of scalar-expr [scalar-expr scalar-expr] ... maybe-else))]{

There are two forms of @tt{CASE} expression, one like Racket's
@racket[cond] and the other like Racket's @racket[case]:

@racketblock[
(case [(= x 0) "zero"] [else "no"])  (code:comment "CASE WHEN x = 0 THEN 'zero' ELSE 'no' END")
(case #:of x [0 "zero"] [else "no"]) (code:comment "CASE x WHEN 0 THEN 'zero' ELSE 'no' END")
]}

@specsubform[(code:line
             (compare-operator scalar-expr #:some table-expr)
             (compare-operator scalar-expr #:all table-expr))]{

Produces an ``all-or-any'' comparison between a scalar (or row)
expression and a table expression.

@racketblock[
(= x #:some (select y #:from ys))   (code:comment "x = SOME (SELECT y FROM ys)")
(< x #:all (select y #:from ys))    (code:comment "x < ALL (select y FROM ys)")
]}

@specsubform[(operator/special scalar-expr ...)]{

Used to represent uses of SQL operators, standard SQL functions that
don't use ordinary function-call notation, and a few other special
cases.

@itemlist[

@item{The @tt{CAST} and @tt{EXTRACT} special functions:

@racketblock[
(cast "2015-03-15" DATE)      (code:comment "CAST('2015-03-15' AS DATE)")
(cast "123" (NUMERIC 5 0))    (code:comment "CAST('123' AS NUMERIC(5, 0))")
(extract YEAR dob)            (code:comment "EXTRACT(YEAR FROM dob)")
]

Note that as above, types and fields are written as ``scalar
expressions'', in a mild abuse of syntax.
}

@item{The @tt{OVERLAY}, @tt{POSITION}, and @tt{SUBSTRING} functions:

@racketblock[
(overlay "abc" "z" 2 1)       (code:comment "OVERLAY('abc' PLACING 'z' FROM 2 FOR 1)")
(position "c" "abc")          (code:comment "POSITION('c' IN 'abc)")
(substring "abc" 2 1)         (code:comment "SUBSTRING('abc' FROM 2 FOR 1)")
]}

@item{The @tt{TRIM} function is written using one of the following variants:

@racketblock[
(trim-leading "z" "zzabc")    (code:comment "TRIM(LEADING 'z' FROM 'zzabc')")
(trim-trailing "z" "abczz")   (code:comment "TRIM(TRAILING 'z' FROM 'abczz')")
(trim-both "z" "zzabczz")     (code:comment "TRIM(BOTH 'z' FROM 'zzabczz')")
]}

@item{The syntax @tt{COUNT(*)} can be written as follows:

@racketblock[
(count-all)                   (code:comment "COUNT(*)")
]}

@item{The chaining arithmetic operators @tt{+}, @tt{-}, @tt{*}, and @tt{/}:

@racketblock[
(+ 1 2 3 4)                   (code:comment "1 + 2 + 3 + 4")
]}

@item{The chaining infix logical operators @tt{AND} and @tt{OR}:

@racketblock[
(and x y z)                   (code:comment "x AND y AND z")
(or x y z)                    (code:comment "x OR y OR z")
]}

@item{The chaining infix binary operator @tt{||} can be written as
@racket[\|\|] or as @racket[||]; the latter reads as the empty symbol.

@racketblock[
(|| lname ", " fname)         (code:comment "lname || ', ' || fname")
(\|\| lname ", " fname)       (code:comment "lname || ', ' || fname")
]}

@item{Any identifier consisting of only characters in
@litchar["~!@#%^&*-_=+|<>?/"] is considered a non-chaining infix
binary operator:

@racketblock[
(< x y)                       (code:comment "x < y")
(%&! 1 2)                     (code:comment "1 %&! 2")
]}

@item{The following operators:

@racketblock[
(not x)                       (code:comment "NOT x")
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
(distinct-from x y)           (code:comment "x DISTINCT FROM y")
(like "abc" "a%")             (code:comment "'abc' LIKE 'a%'")
(ilike "aBC" "ab_")           (code:comment "'aBC' ILIKE 'ab_'")
(similar-to "abc" "(a|z)%")   (code:comment "'abc' SIMILAR TO '(a|z)%'")
]}

@item{Field selection is written as a regular identifier (or @tt{*})
prefixed by a dot.

@racketblock[
(.city state)                 (code:comment "(state).city")
(.* table1)                   (code:comment "(table1).*")
(.*)                          (code:comment "*")
]}

@item{Row constructors (the @tt{ROW} syntax is a PostgreSQL extension):

@racketblock[
(%row 1 2 3)                  (code:comment "(1, 2, 3)")
(row 1 2 3)                   (code:comment "ROW(1, 2, 3)")
]}

@item{Arrays and array indexing (PostgreSQL extension):

@racketblock[
(%array 1 2 3)                (code:comment "ARRAY[1, 2, 3]")
(%ref x 1)                    (code:comment "(x)[1]")
(%ref x 1 2 3)                (code:comment "(x)[1,2,3]")
]}


]}

@specsubform[(name scalar-expr ...)]{

Represents an ordinary function call; no arity checking is done.
@racketblock[
(coalesce x y z)              (code:comment "coalesce(x, y, z)")
]}


@deftogether[[
@defform[(scalar-expr-qq scalar-expr)]
@defproc[(scalar-expr-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for
@svar[scalar-expr].

@examples[#:eval the-eval
(sql-ast->string (scalar-expr-qq mytable.mycolumn))
(sql-ast->string (scalar-expr-qq 42))
(sql-ast->string (scalar-expr-qq "Salutations"))
(sql-ast->string (scalar-expr-qq "a 'tricky' string"))
(sql-ast->string (scalar-expr-qq (log (- 1 p))))
(sql-ast->string (scalar-expr-qq (and (> x 10) (< x 55))))
(sql-ast->string (scalar-expr-qq (coalesce x y z)))
(sql-ast->string (scalar-expr-qq (cast "2015-03-15" DATE)))
(sql-ast->string (scalar-expr-qq (extract YEAR dob)))
(sql-ast->string (scalar-expr-qq (is-null mytable.mycolumn)))
(sql-ast->string (scalar-expr-qq (like ph_num "555-____")))
(sql-ast->string (scalar-expr-qq (|| lname ", " fname)))
]}

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
]]{

Quasiquotation macro and predicate, respectively, for
@svar[table-ref].

@examples[#:eval the-eval
(sql-ast->string (table-ref-qq supplier))
(sql-ast->string (table-ref-qq (as supplier s)))
(sql-ast->string (table-ref-qq (inner-join supplier part #:using supply_id)))
]
}

@deftogether[[
@defform[(table-expr-qq table-expr)]
@defproc[(table-expr-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for
@svar[table-expr].
}

@; ----------------------------------------
@subsection[#:tag "statements"]{SQL Statements}

A statement is one of the four standard DML statements or a @tt{WITH}
statement that combines them with one or more common table
expressions.

@racketgrammar*[
[statement select-statement
           insert-statement
           update-statement
           delete-statement
           with-statement]
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

@bold{With}

@racketgrammar*[

[with-statement (@#,lit{with} maybe-rec ([table-ident/columns statement])
                  statement)]
[maybe-rec (code:line)
           #:recursive]
[table-ident/columns table-ident
                     (table-ident column-ident ...)]
]

@deftogether[[
@defform[(statement-qq statement)]
@defproc[(statement-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for
@svar[statement].

@examples[#:eval the-eval
(sql-ast->string
 (statement-qq (select a b c #:from mytable #:where (> a 10))))
(sql-ast->string
 (statement-qq (insert #:into mytable #:set [a 1] [b 2] [c 3])))
]
}

@; ----------------------------------------
@subsection[#:tag "ddl-statements"]{SQL DDL Statements}

@racketgrammar*[
[ddl-statement create-table-statement
               create-view-statement]

[create-table-statement
    (@#,lit{create-table} maybe-temp table-name
      #:columns column-def ...
      maybe-constraints)
    (@#,lit{create-table} maybe-temp #:as statement)]

[column-def [column-ident type maybe-not-null]]
[maybe-not-null (code:line)
                #:not-null]

[maybe-constraints (code:line)
                   (code:line #:constraints constraint-decl ...)]
[constraint-decl (@#,lit{constraint} constraint-ident constraint)
                       constraint]
[constraint (@#,lit{primary-key} column-ident ...)
                  (@#,lit{unique} column-ident ...)
                  (@#,lit{check} scalar-expr)
                  (@#,lit{foreign-key} column-ident ...
                     #:references table-ident/columns)]

[create-view
    (@#,lit{create-view} maybe-temp view-name
      statement)]

]

@; ----------------------------------------
@subsection[#:tag "dialect"]{SQL Dialect}

@defparam[current-sql-dialect
          dialect
          (or/c symbol? dbsystem? connection?)]{

Controls the default dialect used when converting SQL ASTs to strings
using functions such as @racket[sql-ast->string].

This parameter @bold{does not} affect statement
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
(select a b c #:from mytable #:where (> a 10))
(insert #:into mytable #:set [a 1] [b 2] [c 3])
(insert #:into mytable
        #:from (select a b c 
                       #:from other_table
                       #:where (is-not-null d)))
]}

@deftogether[[
@defform*[[(create-table maybe-temp table-name
              #:columns column-def ...
              maybe-constraints)
           (create-table maybe-temp #:as statement)]]
@defform[(create-view maybe-temp view-name
           statement)]
]]{

Like @racket[select] etc, but for the DDL nonterminals
@svar[create-table-statement] and @svar[create-view-statement],
respectively.

@examples[#:eval the-eval
(create-table numbers
  #:columns [n integer #:not-null] [t text]
  #:constraints
  (primary-key n))
]}

@defproc[(sql-statement? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a statement value returned by one
of the forms in this section such as @racket[select], @racket[#f]
otherwise.
}

@defproc[(sql-statement->string [statement sql-statement?]
                                [dialect (or/c symbol? #f) (current-sql-dialect)])
         string?]{

Produces SQL code as a string for the given @racket[statement]
according to the rules of @racket[dialect].
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
(select a #:from mytable #:where (= b ?))
]

The resulting statement can be used with parameters thus:

@racketblock[
(query-value c (select a #:from mytable #:where (= b ?)) 10)
]

Using the @lit{unquote} form eliminates the need to keep track of
positional parameters; instead, the parameter value is written as a
Racket expression within the statement. It is automatically translated
to SQL code containing placeholders.

@examples[#:eval the-eval
(define b-param 10)
(select a #:from mytable #:where (= b ,b-param))
]

The resulting statement must be called without additional parameters:

@racketblock[
(query-value c (select a #:from mytable #:where (= b ,b-param)))
]

Note that placeholder syntax varies between SQL dialects. We can see
the code a statement produces for a specific dialect by setting the
@racket[current-sql-dialect] parameter:

@interaction[#:eval the-eval
(parameterize ((current-sql-dialect 'postgresql))
  (sql-statement->string (select a #:from mytable #:where (= b ,b-param))))
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
