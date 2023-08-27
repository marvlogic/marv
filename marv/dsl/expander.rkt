#lang racket/base

(require (for-syntax racket/base syntax/parse))

(begin-for-syntax
  (define (c-marv-spec stx)
    (displayln stx)
    (syntax-parse stx
      [(_ DECL ...) #'(begin DECL ...)]
      [else (raise "nowt")]))

  (define (c-decl stx)
    (displayln stx)
    (syntax-parse stx
      [(_ SYM) #'(displayln SYM)]
      [else (raise "nowt-decl")]))

  (define (c-var-decl stx)
    (syntax-parse stx
      [(_ id EXPR) #'(displayln (format "name: ~a ~a" id EXPR))]
      [else (raise "nowt-var-decl")]))

  (define (c-expression stx)
    (displayln stx)
    (syntax-parse stx
      [(_ val) #'val]
      [else (raise "nowt-expression")]))

  ; (require racket/pretty)
  ;  (marv-spec (decl 'xyz) (decl 'pdq))
  ; (marv-spec (decl 'xyz) (decl 'pdq))
  ; (pretty-print (marv-spec (decl 'xyz) (decl 'pdq)))
  )

(define-syntax marv-spec c-marv-spec)
(define-syntax decl c-decl)
(define-syntax var-decl c-var-decl)
(define-syntax expression c-expression)

(provide marv-spec decl var-decl expression)