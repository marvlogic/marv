#lang racket/base

(require "generic-api.rkt")

(provide initialise)

(define PROJECT-ID (make-parameter #f))
(define ACCESS-TOKEN (make-parameter #f))

(define (initialise project-id access-token)
  (initialise-generic headers type-mapper)
  (PROJECT-ID project-id)
  (ACCESS-TOKEN access-token))

(define (headers _) (hash 'Authorization (format "Bearer ~a" (ACCESS-TOKEN))))

(define API
  #hasheq(
   (gcp/bucket . "https://storage.googleapis.com/storage/v1/b?projectId=~a")
   (gcp/vpc . "https://compute.googleapis.com/compute/v1/projects/~a/global/networks")
   ))

(define (type-mapper t) (format (hash-ref API t) (PROJECT-ID)))
