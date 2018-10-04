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
     #f
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
     (f table-expr:values (q (0 "nothing")))
     #f)
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
      (c (c (f scalar:placeholder) c (f scalar:placeholder))))
     #f)
    (q 1 "the loneliest number"))))
 #""
 #"")
((parameterize
  ((current-sql-dialect 'postgresql))
  (print (insert #:into the_numbers #:set (n ,n1) (d ,d1))))
 ((3) 0 () 0 () () (c values c (void)))
 #"(sql-statement \"INSERT INTO the_numbers (n, d) VALUES ($1, $2)\" 1 \"the loneliest number\")"
 #"")
((query-exec
  pgc
  (insert #:into the_numbers #:set (n ?) (d ?))
  (+ 1 1)
  "company")
 ((3) 0 () 0 () () (c values c (void)))
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
((current-sql-dialect #f) ((3) 0 () 0 () () (c values c (void))) #"" #"")
