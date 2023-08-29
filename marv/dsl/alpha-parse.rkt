#lang racket/base

(module reader racket/base

  (require syntax/strip-context)
  (require (rename-in (submod "alpha.rkt" reader)
                      (read-syntax marv-read-syntax)))

  ; (provide read)
  ; (define (read in) (syntax->datum (read-syntax #f in)))

  (require racket/pretty)

  (define (marv-dump-syntax path port)
    (define parse-tree (syntax->datum (marv-read-syntax path port)))
    (strip-context
     #`(module marv-parser racket/base
         (require racket/pretty)
         (pretty-print '#,parse-tree))))

  (provide (rename-out [marv-dump-syntax read-syntax]))
  )
