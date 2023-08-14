#lang racket/base
(require net/http-easy)
(require "hash-utils.rkt")
(require json)

(provide resource-create resource-read resource-state resource-ref)

(define ROOT-API "https://storage.googleapis.com/storage/v1/b")

(define (headers token) (hash 'Authorization (format "Bearer ~a" token)))

(define bucket-request-template #<<EOF
{
    "name": "~a",
    "location": "~a",
    "storageClass": "~a"
}
EOF
  )
(define (resource-create #:project project
                         #:access-token token
                         #:name name
                         #:region region
                         #:storage-class storage-class)

  (define api (format "~a?project=~a" ROOT-API project))
  (define bucket-request (format bucket-request-template name region storage-class))
  (resource-state
   (response-json
    (post api #:headers (headers token) #:json (string->jsexpr bucket-request)))))


(define (resource-read #:project project
                       #:access-token token
                       #:name name)
  (define api (format "~a/~a" ROOT-API name))
  (response-json (get api #:headers (headers token))))

(define (resource-state b) (hash-remove-multi b 'etag 'timeCreated))

(define (resource-ref b)(hash-ref b 'selfLink))
