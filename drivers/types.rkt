#lang racket/base

(require racket/contract)
(require marv/log)
(require uri-template)
(require marv/utils/hash)
(provide (all-defined-out))

(define driver-id/c symbol?)
(define driver/c procedure?)
(define driver-set/c (hash/c driver-id/c driver/c))

(define driver-cmd/c hash?)

; TODO41 - vs config/c ?
(define driver-resp/c hash?)

; TODO41 - right place?
(define api/c hash? )

(define (driver-spec-api ds) (hash-ref ds 'api))
(define (driver-spec-pre-fn ds) (hash-ref ds 'pre (lambda()(lambda(x)x))))
(define (driver-spec-post-fn ds) (hash-ref ds 'post (lambda()(lambda(x y)y))))

; TODO41 - deprecated
; (define (api-request-type api r)
;   (define rt (hash-ref api 'request-type))
;   (cond
;     [(procedure? rt) (rt r)]
;     [else r]))

(define (api-request-body api) (hash-ref api 'request-body (hash)))
(define (api-http-method api) (string->symbol(hash-ref api 'method)))
(define (api-response-type api) (hash-ref api 'response-type))
(define (api-url-template api) (hash-ref api 'url))

(define/contract (api-url a res)
  (api/c hash? . -> . string?)
  (define url (api-url-template a))
  ;TODO41 - are we sure path parameters are always symbols?
  (define config
    (hash-map/copy
     (hash-take res (map string->symbol (variables-of url)))
     (lambda(k v) (values (symbol->string k) v))))
  (displayln (format  " api-url config: ~a ~a" config (map (lambda(x)(equal? "x" x)) (hash-keys config))))
  (expand-template url config))