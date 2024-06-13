#lang racket/base

(require marv/utils/hash)
(require uri-template)

(provide expand-uri uri-vars)

(define (expand-uri uri cfg)
  (define config
    (hash-map/copy
     (hash-take cfg (map string->symbol (variables-of uri)))
     (lambda(k v) (values (symbol->string k) v))))
  (expand-template uri config))

(define (uri-vars str) (map string->symbol (variables-of str)))