#lang info

;; ========================================
;; pkg info

(define collection "sql")
(define deps
  '(["base" #:version "6.3"]
    "rackunit-lib"
    "db-lib"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "db-doc"))

;; ========================================
;; collect info

(define name "sql")
(define scribblings
  '(["sql.scrbl" (#;multi-page)]))
