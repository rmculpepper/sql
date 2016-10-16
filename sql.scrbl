#lang scribble/manual
@(require scribble/manual
          scribble/basic
          scribble/example
          racket/runtime-path
          racket/sandbox
          (for-label racket
                     racket/contract
                     db/base
                     sql))

@title{SQL: A Structured Notation for SQL Statements}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@(define (lit str) (racketfont str))

@(begin
   (define-syntax-rule (interaction e ...) (examples #:label #f e ...))
   (define-runtime-path log-file "private/log-for-sql-docs.rktd")
   (define log-mode 'replay)
   (define (make-pg-eval log-file)
     (let ([ev (make-log-based-eval log-file log-mode)])
       (ev '(require racket/class db sql db/util/postgresql db/util/datetime))
       ev))
   (define db-eval (make-pg-eval log-file)))

@(define the-eval (make-base-eval))
@(the-eval '(require sql))

@defmodule[sql]

This library provides an S-expression notation for a subset of
SQL. It provides forms that produce statements (as opaque values
rather than strings) that can be used directly with Racket's
@racketmodname[db] library. It also provides macros and functions for
creating and manipulating SQL ASTs.

@; ============================================================
@section[#:tag "sql-intro"]{Using the SQL Library}

This library complements the @racketmodname[db] library. The database
library manages connecting to databases and executing queries; this
library helps construction of the queries to execute.

We'll start by going through the examples @secref["intro-basic" #:doc
'(lib "db/scribblings/db.scrbl")] using this library's SQL notation
instead.

@interaction[#:eval db-eval
(require sql db)
(eval:alts
 (define pgc ....)
 (define pgc (dsn-connect 'db-scribble-env)))
]

First we create a temporary table to play around with:

@interaction[#:eval db-eval
(query-exec pgc
  (create-table #:temporary the_numbers
    #:columns [n integer #:not-null] [d varchar]))
(query-exec pgc
  (insert #:into the_numbers #:set [n 0] [d "nothing"]))
]

Let's take a look at the statements that just went by:

@interaction[#:eval db-eval
(create-table #:temporary the_numbers
  #:columns [n integer #:not-null] [d varchar])
(insert #:into the_numbers #:set [n 0] [d "nothing"])
]

Now let's add another row, using ``computed'' values rather than
literals. We can use @racket[unquote] (or @litchar{,}) in a scalar
expression position to insert a Racket value:

@interaction[#:eval db-eval
(define n1 1)
(define d1 "the loneliest number")
(query-exec pgc
  (insert #:into the_numbers #:set [n ,n1] [d ,d1]))
]

Let's look at that last statement:

@interaction[#:eval db-eval
(insert #:into the_numbers #:set [n ,n1] [d ,d1])
]

The @racket[unquote]d expressions turned into parameter placeholders,
and the statement stores their values separately. Strangely, the
placeholders appear as @tt{?}, and PostgreSQL doesn't understand
@tt{?} placeholders; they should have been @tt{$1} and @tt{$2}. But
the statement seems to have worked. What's going on?

We need to set the interactive printing dialect to PostgreSQL. This has no
effect on the query operations; they set the dialect independently
based on the database connection.
@interaction[#:eval db-eval
(parameterize ((current-sql-dialect 'postgresql))
  (print (insert #:into the_numbers #:set [n ,n1] [d ,d1])))
]
And now we see @tt{$1} and @tt{$2} as expected.

We can introduce placeholders explicitly (although @racket[unquote] is
usually more convenient). An explicit placeholder is written
@racket[?], regardless of the dialect. The parameters are given in the
query call as usual:

@interaction[#:eval db-eval
(query-exec pgc
  (insert #:into the_numbers #:set [n ?] [d ?])
  (+ 1 1) "company")
]

It is not currently possible to mix explicit placeholders and
@racket[unquote] parameters:
@interaction[#:eval the-eval
(eval:error
 (query-exec pgc
   (insert #:into the_numbers #:set [n ,3] [d ?])
   "a crowd"))
]

You can, of course, mix constant literals and @racket[unquote]s (or
placeholders).

@interaction[#:eval db-eval
(query-exec pgc
  (insert #:into the_numbers #:set [n 3] [d ,"a crowd"]))
]

@tt{SELECT} statements are constructed similarly, and they follow the
same rules regarding parameters. The statements work the same with all
of the query operations.

@interaction[#:eval db-eval
(query pgc
  (select n d #:from the_numbers #:where (= (% n 2) 0)))
(query-rows pgc
  (select n d #:from the_numbers #:where (= (+ n n) (* n n))))
(query-row pgc
  (select n d #:from the_numbers #:where (< n 1)))
(query-list pgc
  (select d #:from the_numbers #:where (= 0 (% n 2))))
(query-value pgc
  (select (string_agg d ", ") #:from the_numbers #:where (= 0 (% n 2))))
]

There are S-expression notations for many common SQL operators and
expression forms. See @secref["scalar-exprs"] for details.

The rest of this manual uses the default SQL1992 dialect for printing
results:
@interaction[#:eval db-eval
(current-sql-dialect #f)
]


@; ============================================================
@section[#:tag "statement-forms"]{Statement Forms}

The macros in this section create statement values suitable for
passing to the query functions of the @racketmodname[db]
library. These statement values satisfy the @racketmodname[db]
library's @racket[statement?] predicate. They are different from the
@svar[statement] ASTs produced by @racket[statement-qq].

The printing of a statement value is controlled by
@racket[(current-sql-dialect)], but the code it generates when passed
to a query function is determined by the dialect of the connection the
query is performed on.

@defform*[[(sql statement)
           (sql ddl-statement)]]{

Produces a statement value that can be passed to a @racketmodname[db]
query function. The syntax corresponds to the syntax of the
@svar[statement] or @svar[ddl-statement] nonterminals from
@secref["sql-syntax"].

@examples[#:eval the-eval
(sql (select a b c #:from mytable #:where (> a 10)))
(sql (insert #:into mytable #:set [a 1] [b 2] [c 3]))
(sql (create-table numbers
       #:columns [n integer #:not-null] [t text]
       #:constraints (primary-key n)))
]}

@deftogether[[
@defform*[[(select select-item ... select-clause ...)
           (select select-clause ...)]]

@defform*[[(insert #:into table-name assign-clause)
           (insert #:into table-name maybe-columns #:from table-expr)]]

@defform[(update table-name assign-clause maybe-where)]

@defform[(delete #:from table-name maybe-where)]
]]{

Like @racket[sql], but specialized to the syntax of the
@svar[select-statement], @svar[insert-statement],
@svar[update-statement], and @svar[delete-statement] nonterminals from
@secref["sql-syntax"], respectively.

@examples[#:eval the-eval
(select a b c #:from mytable #:where (> a 10))
(insert #:into mytable #:set [a 1] [b 2] [c 3])
(insert #:into mytable
        #:from (select a b c 
                       #:from other_table
                       #:where (is-not-null d)))
]

Equivalent to 
@racketblock[
(sql (@#,lit{select} select-item ... select-clause ...))
(sql (@#,lit{insert} #:into table-name assign-clause))
]
and so forth.
}


@deftogether[[
@defform*[[(create-table maybe-temp table-name
              #:columns column-def ...
              maybe-constraints)
           (create-table maybe-temp table-name
              #:as statement)]]
@defform[(create-view maybe-temp view-name
           statement)]
]]{

Like @racket[sql], but specialized to the syntax of the DDL
@svar[create-table-statement] and @svar[create-view-statement],
respectively.

@examples[#:eval the-eval
(create-table numbers
  #:columns [n integer #:not-null] [t text]
  #:constraints (primary-key n))
]

Equivalent to
@racketblock[
(sql (@#,lit{create-table} maybe-temp table-name
       #:columns column-def ...
       maybe-constraints))
]
and so forth.
}

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
                                     table-ref-ast? statement-ast? ddl-ast?)]
                          [dialect (or/c symbol? #f) (current-sql-dialect)])
         string?]{

Produces SQL code as a string for the given AST to a string according
to the rules of @racket[dialect]. Examples are given throughout the
following sections.
}

@; ----------------------------------------
@subsection[#:tag "names"]{SQL Names and Identifiers}

A name is either an unqualified identifier or an identifier qualified
with another name, which depending on its usage might represent a
catalog, schema, table, range variable, etc.

Concrete SQL has both unquoted and quoted identifiers. Different SQL
environments (eg, database backends) have different restrictions on
unquoted identifiers, regarding illegal characters and reserved
words. Most (but not all) systems also apply some case-folding rule to
unquoted identifiers (eg, PostgreSQL converts to lowercase, some
others convert to uppercase).

Similarly, this library has both ``tagged'' and ``untagged'' notations
for identifiers and names. Untagged identifiers are written as raw
symbols; they are short and convenient, but they run the risk of
confusion with operators and special symbols used by this
library. Examples of special symbols include @lit{select}, @lit{as},
and @lit{from}. Examples of identifiers containing operator characters
include @tt{hello-goodbye} and @tt{first/last}. These identifiers must
be written in tagged form.

@racketgrammar*[

[ident symbol
       (@#,lit{Ident:} string)
       (@#,lit{Ident:} symbol)]

[name symbol
      ident
      (@#,lit{Name:} name ...+)]

]

@specsubform[(@#,lit{Ident:} symbol)]{

Unquoted if possible; case-folded and quoted according the SQL
dialect's rules if @racket[symbol] is a reserved word or contains
illegal characters.

@racketblock[
(Ident: MyTable)                (code:comment "MyTable")
(Ident: Select)                 (code:comment "\"SELECT\"")
(Ident: a+b.c)                  (code:comment "\"a+b.c\"")
]}

@specsubform[(@#,lit{Ident:} string)]{

Always quoted without case-folding.

@racketblock[
(Ident: "MyTable")              (code:comment "\"MyTable\"")
(Ident: "Select")               (code:comment "\"Select\"")
(Ident: "x1.$!!")               (code:comment "\"x1.$!!\"")
]}

@specsubform[(@#,lit{Name:} name ...+)]{

Qualified name; each name except the last qualifies the name to its
right.

@racketblock[
(Name: x y z)                   (code:comment "x.y.z")
(Name: x y.z)                   (code:comment "x.y.z")
(Name: x (Ident: y.z))          (code:comment "x.\"y.z\"")
]}

@specsubform[symbol]{

Must not be a special symbol; otherwise an error is raised.

Equivalent to @racket[(@#,lit{Ident:} symbol)] if @racket[symbol] contains
no dot (@litchar{.}) characters and matches the pattern
@racket[#px"^(?:\\p{L}|_)(?:\\p{L}|\\p{N}|[_$])*$"]---that is, a letter
or underscore followed by zero or more letters, numbers, underscores,
and dollar signs.

If @racket[symbol] consists of dot-separated @racket[_part]s satisfying
the rule above, it is equivalent to @racket[(Name: _part ...)].

@racketblock[
MyTable                         (code:comment "MyTable")
x.y.z                           (code:comment "x.y.z")
x.select.as                     (code:comment "x.\"SELECT\".\"AS\"")
]}

Because case-folding behavior is system-dependent, it is wisest to
either always quote a given name or never quote it.

@deftogether[[
@defform[(ident-qq ident)]
@defproc[(ident-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for @svar[ident].

@examples[#:eval the-eval
(sql-ast->string (ident-qq MyTable))
(sql-ast->string (ident-qq (Ident: MyTable)))
(sql-ast->string (ident-qq (Ident: "MyTable")))

(sql-ast->string (ident-qq Select))
(sql-ast->string (ident-qq (Ident: Select)))
(sql-ast->string (ident-qq (Ident: "Select")))
(sql-ast->string (ident-qq (Ident: a+b.c)))

(eval:error (sql-ast->string (ident-qq select)))
(eval:error (sql-ast->string (ident-qq a+b.c)))
]}

@deftogether[[
@defform[(name-qq name)]
@defproc[(name-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for @svar[name].

@examples[#:eval the-eval
(sql-ast->string (name-qq (Name: x y z)))
(sql-ast->string (name-qq (Name: x.y z)))
(sql-ast->string (name-qq x.y.z))
(sql-ast->string (name-qq x.select.as))
]}


@; ----------------------------------------
@subsection[#:tag "scalar-exprs"]{SQL Scalar Expressions}

A scalar expression is either a name, a literal integer or string value,
or an application of some function or operator. Note: not every kind of
expression is supported in every SQL dialect.

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
             (name scalar-expr ...)
             table-expr
             (operator/special scalar-expr ...)
             ?
             (@#,lit{unquote} racket-expr)]
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

@specsubform[(name scalar-expr ...)]{

Represents an ordinary function call; no arity checking is done.
@racketblock[
(coalesce x y z)              (code:comment "coalesce(x, y, z)")
]}

@specsubform[table-expr]{

Represents a subquery; the query must return at most one row.

@racketblock[
(select y #:from ys #:where (x = 0))  (code:comment "(SELECT y FROM ys WHERE x = 0)")
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
@subsubsection[#:tag "special-operators"]{Special Scalar Expressions}

@specsubform[(operator/special scalar-expr ...)]{

This function-like syntax is used to represent uses of SQL operators,
standard SQL functions that don't use ordinary function-call notation,
and a few other special cases, listed below.
}

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
]


@; ----------------------------------------
@subsubsection[#:tag "unquote"]{Placeholders and Unquoted Parameters}

There are two variants of @svar[scalar-expr] that enable
the construction of parameterized queries. The first is the placeholder,
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
    (@#,lit{create-table} maybe-temp table-name
      #:as statement)]

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

@deftogether[[
@defform[(ddl-qq ddl-statement)]
@defproc[(ddl-ast? [v any/c]) boolean?]
]]{

Quasiquotation macro and predicate, respectively, for
@svar[ddl-statement].
}

@; ----------------------------------------
@subsection[#:tag "dialect"]{SQL Dialect}

@defparam[current-sql-dialect
          dialect
          (or/c symbol? #f)]{

Controls the default dialect used when converting SQL ASTs to strings
using functions such as @racket[sql-ast->string].

This parameter @bold{does not} affect statement
(@racket[sql-statement?]) values used with connection query methods;
generation of SQL code for a query method automatically uses the
dialect associated with the connection the query is performed on.
}


@; ============================================================
@subsection[#:tag "escapes"]{Dynamic Statement Composition and SQL Injection}

This library allows the dynamic composition of statements and the
injection of SQL text using the following extensions to the SQL
grammar.

@bold{Warning:} Never use the @lit{INJECT} forms to include SQL
computed from an untrusted source. Use placeholders or
@racket[unquote] parameters instead; see @secref["unquote"].

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


@(close-eval db-eval)
