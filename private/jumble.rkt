#lang racket/base
(require racket/list)
(provide (all-defined-out))

;; ============================================================
;; Util: jumbles

;; A Jumble is String | (Listof Jumble)
(define (J . args) args)
(define (J-join js sep) (add-between js sep))

(define (jumble->string j)
  (define out (open-output-string))
  (let loop ([j j])
    (cond [(string? j) (write-string j out)]
          [else (for-each loop j)]))
  (get-output-string out))
