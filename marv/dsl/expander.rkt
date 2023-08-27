#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide marv-spec decl var-decl expression)

(define-syntax (marv-spec stx)
  (syntax-parse stx
    [(_ DECL ...) #'(begin DECL ...)]
    [else (raise "nowt")]))

(define-syntax (decl stx)
  (syntax-parse stx
    [(_ SYM) #'(displayln SYM)]
    [else (raise "nowt-decl")]))

(define-syntax (var-decl stx)
  (syntax-parse stx
    [(_ id EXPR) #'(displayln (format "name: ~a ~a" id EXPR))]
    [else (raise "nowt-var-decl")]))

(define-syntax (expression stx)
  (syntax-parse stx
    [(_ val) #'val]
    [else (raise "nowt-expression")]))

; (require racket/pretty)
;  (marv-spec (decl 'xyz) (decl 'pdq))
; (marv-spec (decl 'xyz) (decl 'pdq))
; (pretty-print (marv-spec (decl 'xyz) (decl 'pdq)))