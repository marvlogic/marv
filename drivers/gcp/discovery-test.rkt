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

(define (resources rs)
  (define api-id (symbol->string (join-symbols (list rs 'insert))))
  (define (res-acc racc)
    (hash 'resources
          (match racc
            [(list r)
             (hash r
                   (hash 'methods
                         (hash 'insert (hash 'id api-id 'some-api 'xx))))]
            [(list r rs ...)
             (hash r (res-acc rs))])))
  (res-acc (cdr (split-symbol rs))))

(define compute-api (disc-doc (resources 'compute.networks)))
(define iam-api (disc-doc (resources 'iam.projects.serviceAccounts.keys)))

(check-true (disc-api? (api-for-type-op compute-api 'compute.networks.insert)))
(check-true (disc-api? (api-for-type-op iam-api 'iam.projects.serviceAccounts.keys.insert)))

; TODO14
; (check-exn exn:fail? (lambda()(api-for-type-op iam-api 'projects.notfound.get)))