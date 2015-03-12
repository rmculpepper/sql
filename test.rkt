#lang racket
(require "main.rkt")

(define (test stx)
  (define s (parse-statement stx))
  (pretty-print (syntax->datum stx))
  ;; (pretty-print s)
  (printf "~a\n\n" (statement->string s)))

(define (err stx)
  (with-handlers ([values (lambda (e) (printf "OK (error raised as expected)\n\n"))])
    (pretty-print (syntax->datum stx))
    (parse-statement stx)))

(test #'(select a b (+ c 1) (as (* d 2) dd)))
(test #'(select * #:from T))
(test #'(select #:from T #:values a b c))
(test #'(select #:from T #:values a b c #:where (> a b)))
(test #'(select #:from (as T A) #:values A.a A.b #:where (> A.a A.b)))
(test #'(select #:values * #:from (select #:values a #:from A)))
(test #'(select #:values * #:from A
                #:group-by a #:having (= b 2)
                #:order-by a #:asc #:limit 1))
(test #'(select #:values *
                #:from (as (select #:values a #:from A) T)))
(test #'(select #:values a b
                #:from (inner-join A B #:using (a))
                #:where (= b ?)))

(err #'(select a$))
(err #'(select * from T))

(test #'(insert #:into T (a b c) #:from (select * #:from S #:where (= d ?))))
(test #'(insert #:into T (a b c) #:values 1 2 3))

(test #'(update T #:set [a (+ a 1)] [b (- b 1)] #:where (< T.a 0)))
