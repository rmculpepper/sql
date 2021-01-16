#lang racket/base
(require (for-syntax racket/base)
         rackunit
         racket/pretty
         "main.rkt")

(define check-dialects (make-parameter '(#f postgresql mysql sqlite3)))

(define-syntax (-test stx)
  (syntax-case stx ()
    [(_ orig body ...)
     #`(test-case (format "~a:~a ~.s" #,(syntax-line #'orig) #,(syntax-column #'orig) 'orig)
         body ...)]))

(define-syntax-rule (-test-ast* ([s str] ...) qq ast?)
  (let ()
    (define (do-test ast xstr)
      (check-pred ast? ast)
      (for ([dialect (check-dialects)] [i (in-naturals)])
        (parameterize ((current-sql-dialect dialect))
          (when (and (zero? i) xstr)
            (check-equal? (sql-ast->string ast) xstr))
          (check-pred string? (sql-ast->string ast)))))
    (-test s (do-test (qq s) str))
    ...))

;; ----------------------------------------
;; Scalar Expressions

(define-syntax-rule (test-se* [s str] ...)
  (-test-ast* ([s str] ...) scalar-expr-qq scalar-expr-ast?))

(test-se*
 [1         "1"]
 [-1        "-1"]
 [a         "a"]
 [a.b       "a.b"]
 [a.B       "a.B"]
 [(f x y)   "f(x, y)"]
 [(exists (select 1 #:from t)) "EXISTS (SELECT 1 FROM t)"]
 [(in x #:values 1 2 3) "(x IN (1, 2, 3))"]
 [(in x #:from (select y #:from ys))
  "(x IN (SELECT y FROM ys))"]
 [(case [(= x 0) "zero"] [else "nope"])
  "CASE WHEN (x = 0) THEN 'zero' ELSE 'nope' END"]
 [(case #:of x [0 "zero"] [else "nope"])
  "CASE x WHEN 0 THEN 'zero' ELSE 'nope' END"]
 [(coalesce x y) "coalesce(x, y)"]
 ;; Special scalar expressions
 [(cast "123" INTEGER) "CAST('123' AS \"INTEGER\")"]
 [(substring "abc" 2 1) "SUBSTRING('abc' FROM 2 FOR 1)"]
 [(trim-leading "z" "zzabc") "TRIM(LEADING 'z' FROM 'zzabc')"]
 [(trim-trailing "z" "abczz") "TRIM(TRAILING 'z' FROM 'abczz')"]
 [(trim-both "z" "zzabczz") "TRIM(BOTH 'z' FROM 'zzabczz')"]
 [(count-all) "COUNT(*)"]
 [(+ 1 2)   "(1 + 2)"]
 [(+ 1 2 3) "(1 + 2 + 3)"]
 [(and x y) "(x AND y)"]
 [(and x y z) "(x AND y AND z)"]
 [(or x y z) "(x OR y OR z)"]
 [(\|\| lname ", " fname) "(lname || ', ' || fname)"]
 [(= 1 2)   "(1 = 2)"]
 [(< 1 2)   "(1 < 2)"]
 [(not x)   "(NOT x)"]
 [(is-null x) "(x IS NULL)"]
 [(is-not-null x) "(x IS NOT NULL)"]
 [(collate x utf8) "(x COLLATE utf8)"]
 [(between-and x 1 10) "(x BETWEEN 1 AND 10)"]
 [(like x "a%") "(x LIKE 'a%')"]
 [(.* state)    "(state).*"]
 [(.*)          "*"]
 [(count (.*))  "count(*)"]
 [(%row 1 2 3)  "(1, 2, 3)"]

 ;; etc
 [((Name: position) 1 2 3) "position(1, 2, 3)"] ;; not POSITION(1 IN 2)
 )

(parameterize ((check-dialects '(#f postgresql mysql)))
  (test-se*
   [(= x #:some (select y #:from ys)) "(x = SOME (SELECT y FROM ys))"]
   [(select y #:from ys) "(SELECT y FROM ys)"]
   [(cast "2000-01-01" DATE) "CAST('2000-01-01' AS \"DATE\")"]
   [(extract YEAR dob) "EXTRACT(\"YEAR\" FROM dob)"]
   [(overlay "abc" "z" 2 1) "OVERLAY('abc' PLACING 'z' FROM 2 FOR 1)"]
   [(position "c" "abc") "POSITION('c' IN 'abc')"]
   ;; [(%&! 1 2) "(1 %&! 2)"]
   [(is-true x) "(x IS TRUE)"]
   [(is-not-true x) "(x IS NOT TRUE)"]
   [(is-false x) "(x IS FALSE)"]
   [(is-not-false x) "(x IS NOT FALSE)"]
   [(is-unknown x) "(x IS UNKNOWN)"]
   [(is-not-unknown x) "(x IS NOT UNKNOWN)"]
   [(distinct-from x y) "(x DISTINCT FROM y)"]
   [(ilike x "ab_") "(x ILIKE 'ab_')"]
   [(similar-to x "(az)%") "(x SIMILAR TO '(az)%')"]
   [(.city state) "(state).city"]
   ))

(parameterize ((check-dialects '(postgresql)))
  (test-se*
   [(row 1 2 3)   "ROW(1, 2, 3)"]
   [(%array 1 2 3) "ARRAY[1, 2, 3]"]
   [(%ref x 1)     "(x)[1]"]
   [(%ref x 1 2 3) "(x)[1, 2, 3]"]
   ))


;; ----------------------------------------
;; Table Refs

(define-syntax-rule (test-tr* [s str] ...)
  (-test-ast* ([s str] ...) table-ref-qq table-ref-ast?))

(test-tr*
 [tbl "tbl"]
 [qual.tbl "qual.tbl"]
 [(as tbl alias) "tbl AS alias"])

;; ----------------------------------------
;; Table Expressions

(define-syntax-rule (test-te* [s str] ...)
  (-test-ast* ([s str] ...) table-expr-qq table-expr-ast?))

(test-te*
 [(cross-join t1 t2) "t1 CROSS JOIN t2"]
 [(inner-join t1 t2 #:natural) "t1 NATURAL INNER JOIN t2"]
 [(values 1 2 3) "VALUES (1, 2, 3)"]
 [(values* (1 2 3) (4 5 6)) "VALUES (1, 2, 3), (4, 5, 6)"]
 [(select y #:from ys) "SELECT y FROM ys"]
 [(select y #:from ys #:limit 1) "SELECT y FROM ys LIMIT 1"]
 [(union (select x #:from xs) (select y #:from ys) #:all)
  "SELECT x FROM xs UNION ALL SELECT y FROM ys"])

;; ------------------------------------------------------------
;; Statements

(define-syntax-rule (test-stmt* s ...)
  (let ()
    (define (do-test stmt)
      (for ([dialect (check-dialects)])
        (check-pred sql-statement? stmt)
        (parameterize ((current-sql-dialect dialect))
          (check-pred string? (sql-statement->string stmt)))))
    (-test s) ...))

(test-stmt*
 (select a b (+ c 1) (as (* d 2) dd))
 (select * #:from T)
 (select #:from T #:values a b c)
 (select #:from T #:values a b c #:where (> a b))
 (select #:from (as T A) #:values A.a A.b #:where (> A.a A.b))
 (select #:values * #:from (select #:values a #:from A))
 (select #:values * #:from A
         #:group-by a #:having (= b 2)
         #:order-by a #:asc #:limit 1)
 (select #:values *
         #:from (as (select #:values a #:from A) T))
 (select #:values a b
         #:from (inner-join A B #:using a)
         #:where (= b ?))
 (insert #:into T #:columns a b c #:from (select * #:from S #:where (= d ?)))
 (insert #:into T #:set [a 1] [b 2] [c 3])
 (update T #:set [a (+ a 1)] [b (- b 1)] #:where (< T.a 0))
 (with #:recursive
        ([(cnt x) (union
                   (select 1)
                   (select (+ 1 x)
                           #:from cnt))])
        (select x #:from cnt)))

(define-syntax-rule (test-stmt-err* s ...)
  (begin (-test (check-exn #rx"" (lambda () (convert-syntax-error s)))) ...))

(test-stmt-err*
 (select a$)
 (select * from T)
 (select * #:from (values* (1 2) ("single"))))


;; Bad error for: (select x #:from ,"foo") --- why?
