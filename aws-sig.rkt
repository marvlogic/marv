#lang racket/base

(require sha)

(define (hmac k d) (hmac-sha256 k (string->bytes/utf-8 d)))

; https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html#signing-request-intro

(define dates "20230413")
(define secret-key "abcdef")
(define region "eu-west-1")
(define service "ec2")
(define seed (hmac (string->bytes/utf-8 (string-append "AWS4" secret-key)) dates))

(define signing-key (hmac (hmac (hmac seed region) service) "aws4_request"))

(bytes->hex-string
 (foldl (lambda(d k) (hmac k d))
        (string->bytes/utf-8 (string-append "AWS4" secret-key))
        (list dates region service "aws4_request")))