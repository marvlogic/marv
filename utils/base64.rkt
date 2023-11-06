#lang racket/base

(require net/base64)
(require racket/string)

(provide b64enc b64dec)

(define (b64enc s) (string-replace ((compose1 bytes->string/utf-8 base64-encode string->bytes/utf-8) s) "\r\n" ""))
(define b64dec (compose1 bytes->string/utf-8 base64-decode string->bytes/utf-8))