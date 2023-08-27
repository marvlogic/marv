#lang racket/base

(module reader racket/base
  (require syntax/strip-context)
  (require "parser.rkt" "tokenizer.rkt")

  (provide read-syntax)
  ; (define (read in)
  ;   (syntax->datum
  ;    (literal-read-syntax #f in)))


  (define (read-syntax path port)
    (define parse-tree (parse path (make-tokenizer port path)))
    (strip-context
     #`(module marv-top racket/base
         (require marv/dsl/expander)
         #,parse-tree))))