;; This file was created by make-log-based-eval
((require racket/class db sql db/util/postgresql db/util/datetime)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((require sql db) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define pgc (dsn-connect 'db-scribble-env))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec
  pgc
  (create-table
   #:temporary
   the_numbers
   #:columns
   (n integer #:not-null)
   (d varchar)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc (insert #:into the_numbers #:set (n 0) (d "nothing")))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((create-table
  #:temporary
  the_numbers
  #:columns
  (n integer #:not-null)
  (d varchar))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     ddl:create-table
     (f id:normal the_numbers)
     #t
     (c
      (f column (f id:normal n) (f id:normal integer) #t)
      c
      (f column (f id:normal d) (f id:normal varchar) #f))
     ())
    #f)))
 #""
 #"")
((insert #:into the_numbers #:set (n 0) (d "nothing"))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:insert
     (f id:normal the_numbers)
     (c (f id:normal n) c (f id:normal d))
     (f table-expr:values (q (0 "nothing"))))
    #f)))
 #""
 #"")
((define n1 1) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((define d1 "the loneliest number")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc (insert #:into the_numbers #:set (n ,n1) (d ,d1)))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((insert #:into the_numbers #:set (n ,n1) (d ,d1))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:insert
     (f id:normal the_numbers)
     (c (f id:normal n) c (f id:normal d))
     (f
      table-expr:values
      (c (c (f scalar:placeholder) c (f scalar:placeholder)))))
    (q 1 "the loneliest number"))))
 #""
 #"")
((current-sql-dialect 'postgresql)
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((insert #:into the_numbers #:set (n ,n1) (d ,d1))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:insert
     (f id:normal the_numbers)
     (c (f id:normal n) c (f id:normal d))
     (f
      table-expr:values
      (c (c (f scalar:placeholder) c (f scalar:placeholder)))))
    (q 1 "the loneliest number"))))
 #""
 #"")
((query-exec
  pgc
  (insert #:into the_numbers #:set (n ?) (d ?))
  (+ 1 1)
  "company")
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query-exec pgc (insert #:into the_numbers #:set (n ,3) (d ?)) "a crowd")
 ((3)
  0
  ()
  0
  ()
  ()
  (q
   exn
   "insert: cannot use both placeholders and unquoted values\n  in: (insert #:into the_numbers #:set (n (unquote 3)) (d ?))"))
 #""
 #"")
((query-exec pgc (insert #:into the_numbers #:set (n 3) (d ,"a crowd")))
 ((3) 0 () 0 () () (c values c (void)))
 #""
 #"")
((query pgc (select n d #:from the_numbers #:where (= (% n 2) 0)))
 ((3)
  1
  (((lib "db/private/generic/interfaces.rkt")
    .
    deserialize-info:rows-result-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (c
     (c (c name u . "n") q (typeid . 23) (type-size . 4) (type-mod . -1))
     c
     (c (c name u . "d") q (typeid . 1043) (type-size . -1) (type-mod . -1)))
    (c (v! 0 (u . "nothing")) c (v! 2 (u . "company"))))))
 #""
 #"")
((query-rows pgc (select n d #:from the_numbers #:where (= (+ n n) (* n n))))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (c (v! 0 (u . "nothing")) c (v! 2 (u . "company")))))
 #""
 #"")
((query-row pgc (select n d #:from the_numbers #:where (< n 1)))
 ((3) 0 () 0 () () (c values c (v! 0 (u . "nothing"))))
 #""
 #"")
((query-list pgc (select d #:from the_numbers #:where (= 0 (% n 2))))
 ((3) 0 () 0 () () (c values c (c (u . "nothing") c (u . "company"))))
 #""
 #"")
((query-value
  pgc
  (select (string_agg d ", ") #:from the_numbers #:where (= 0 (% n 2))))
 ((3) 0 () 0 () () (c values c (u . "nothing, company")))
 #""
 #"")
((sql-ast->string (name-qq mytable.mycolumn))
 ((3) 0 () 0 () () (c values c (u . "mytable.mycolumn")))
 #""
 #"")
((sql-ast->string (name-qq (Name: mytable MYCOLUMN)))
 ((3) 0 () 0 () () (c values c (u . "mytable.MYCOLUMN")))
 #""
 #"")
((sql-ast->string (name-qq (Name: (Ident: "MyTable") (Ident: "MyColumn"))))
 ((3) 0 () 0 () () (c values c (u . "\"MyTable\".\"MyColumn\"")))
 #""
 #"")
((sql-ast->string (name-qq table.mycolumn))
 ((3) 0 () 0 () () (c values c (u . "\"table\".mycolumn")))
 #""
 #"")
((sql-ast->string (name-qq select.insert))
 ((3) 0 () 0 () () (c values c (u . "\"select\".insert")))
 #""
 #"")
((sql-ast->string (name-qq (Name: (Ident: select) (Ident: insert))))
 ((3) 0 () 0 () () (c values c (u . "\"select\".insert")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq mytable.mycolumn))
 ((3) 0 () 0 () () (c values c (u . "mytable.mycolumn")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq 42))
 ((3) 0 () 0 () () (c values c (u . "42")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq "Salutations"))
 ((3) 0 () 0 () () (c values c (u . "'Salutations'")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq "a 'tricky' string"))
 ((3) 0 () 0 () () (c values c (u . "'a ''tricky'' string'")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (log (- 1 p))))
 ((3) 0 () 0 () () (c values c (u . "log((1 - p))")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (and (> x 10) (< x 55))))
 ((3) 0 () 0 () () (c values c (u . "((x > 10) AND (x < 55))")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (coalesce x y z)))
 ((3) 0 () 0 () () (c values c (u . "coalesce(x, y, z)")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (cast "2015-03-15" DATE)))
 ((3) 0 () 0 () () (c values c (u . "CAST('2015-03-15' AS DATE)")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (extract YEAR dob)))
 ((3) 0 () 0 () () (c values c (u . "EXTRACT(YEAR FROM dob)")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (is-null mytable.mycolumn)))
 ((3) 0 () 0 () () (c values c (u . "(mytable.mycolumn IS NULL)")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (like ph_num "555-____")))
 ((3) 0 () 0 () () (c values c (u . "(ph_num LIKE '555-____')")))
 #""
 #"")
((sql-ast->string (scalar-expr-qq (|| lname ", " fname)))
 ((3) 0 () 0 () () (c values c (u . "(lname || ', ' || fname)")))
 #""
 #"")
((sql-ast->string (table-ref-qq supplier))
 ((3) 0 () 0 () () (c values c (u . "supplier")))
 #""
 #"")
((sql-ast->string (table-ref-qq (as supplier s)))
 ((3) 0 () 0 () () (c values c (u . "supplier AS s")))
 #""
 #"")
((sql-ast->string (table-ref-qq (inner-join supplier part #:using supply_id)))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (u . "supplier INNER JOIN part USING (supply_id)")))
 #""
 #"")
((sql-ast->string
  (statement-qq (select a b c #:from mytable #:where (> a 10))))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (u . "SELECT a, b, c FROM mytable WHERE (a > 10)")))
 #""
 #"")
((sql-ast->string
  (statement-qq (insert #:into mytable #:set (a 1) (b 2) (c 3))))
 ((3)
  0
  ()
  0
  ()
  ()
  (c values c (u . "INSERT INTO mytable (a, b, c) VALUES (1, 2, 3)")))
 #""
 #"")
((select a b c #:from mytable #:where (> a 10))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:select
     (c (f id:normal a) c (f id:normal b) c (f id:normal c))
     (c (f table-ref:name (f id:normal mytable)))
     (c (f scalar:app > (c (f id:normal a) q 10)))
     ()
     ()
     #f)
    #f)))
 #""
 #"")
((insert #:into mytable #:set (a 1) (b 2) (c 3))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:insert
     (f id:normal mytable)
     (c (f id:normal a) c (f id:normal b) c (f id:normal c))
     (f table-expr:values (q (1 2 3))))
    #f)))
 #""
 #"")
((insert
  #:into
  mytable
  #:from
  (select a b c #:from other_table #:where (is-not-null d)))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:insert
     (f id:normal mytable)
     #f
     (f
      table-expr:select
      (f
       statement:select
       (c (f id:normal a) c (f id:normal b) c (f id:normal c))
       (c (f table-ref:name (f id:normal other_table)))
       (c (f scalar:app is-not-null (c (f id:normal d))))
       ()
       ()
       #f)))
    #f)))
 #""
 #"")
((create-table
  numbers
  #:columns
  (n integer #:not-null)
  (t text)
  #:constraints
  (primary-key n))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     ddl:create-table
     (f id:normal numbers)
     #f
     (c
      (f column (f id:normal n) (f id:normal integer) #t)
      c
      (f column (f id:normal t) (f id:normal text) #f))
     (c (f constraint:primary-key (c (f id:normal n)))))
    #f)))
 #""
 #"")
((select a #:from mytable #:where (= b ?))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:select
     (c (f id:normal a))
     (c (f table-ref:name (f id:normal mytable)))
     (c (f scalar:app = (c (f id:normal b) c (f scalar:placeholder))))
     ()
     ()
     #f)
    #f)))
 #""
 #"")
((define b-param 10) ((3) 0 () 0 () () (c values c (void))) #"" #"")
((select a #:from mytable #:where (= b ,b-param))
 ((3)
  1
  (((lib "sql/private/syntax.rkt") . deserialize-info:sql-statement-v0))
  0
  ()
  ()
  (c
   values
   c
   (0
    (f
     statement:select
     (c (f id:normal a))
     (c (f table-ref:name (f id:normal mytable)))
     (c (f scalar:app = (c (f id:normal b) c (f scalar:placeholder))))
     ()
     ()
     #f)
    (q 10))))
 #""
 #"")
((parameterize
  ((current-sql-dialect 'postgresql))
  (sql-statement->string (select a #:from mytable #:where (= b ,b-param))))
 ((3) 0 () 0 () () (c values c (u . "SELECT a FROM mytable WHERE (b = $1)")))
 #""
 #"")
