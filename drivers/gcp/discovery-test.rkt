#lang racket/base

(require rackunit)
(require racket/match)
(require marv/core/globals)
(require racket/string)
(require/expose
 marv/drivers/gcp/discovery
 (disc-doc
  disc-doc-root
  disc-api disc-api?
  api-for-type-op))

(define methods-stub #hasheq((methods . #hasheq((get . some-api)))))

(define (resources rs)
  (define api-id (join-symbols (list rs 'get)))
  (define (res-acc racc)
    (hash 'resources
          (match racc
            [(list r)
             (hash r
                   (hash 'methods
                         (hash 'get (hash 'id api-id 'some-api 'xx))))]
            [(list r rs ...)
             (hash r (res-acc rs))])))
  (res-acc (cdr (split-symbol rs))))

(define compute-api (disc-doc (resources 'compute.networks)))
(define iam-api (disc-doc (resources 'iam.projects.serviceAccounts.keys)))

(check-true (disc-api? (api-for-type-op compute-api 'compute.networks.get)))
(check-true (disc-api? (api-for-type-op iam-api 'iam.projects.serviceAccounts.keys.get)))

; TODO14
; (check-exn exn:fail? (lambda()(api-for-type-op iam-api 'projects.notfound.get)))