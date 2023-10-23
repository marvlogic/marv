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
  api-request-type
  api-response-type
  api-resource
  api-schema
  api-for-type-op))

'discovery-test

(define (mk-disc rs)
  (define api-id (symbol->string (join-symbols (list rs 'insert))))
  (define (res-acc racc)
    (hash 'resources
          (match racc
            [(list r)
             (hash r (hash
                      'methods (hash
                                'insert (hash 'id api-id
                                              'request #hasheq(($ref . "ReqBody"))
                                              'response #hasheq(($ref . "RespBody"))))))]
            [(list r rs ...)
             (hash r (res-acc rs))])))
  (hash-set (res-acc (cdr (split-symbol rs)))
            'schemas
            #hasheq((ReqBody
                     . #hasheq((properties . #hasheq((attr1 . value)(attr2 . value))))))))

(define compute-doc (disc-doc (mk-disc 'compute.networks)))
(define iam-doc (disc-doc (mk-disc 'iam.projects.serviceAccounts.keys)))

(check-true (disc-api? (api-for-type-op compute-doc 'compute.networks.insert)))
(check-true (disc-api? (api-for-type-op iam-doc 'iam.projects.serviceAccounts.keys.insert)))
(check-exn exn:fail? (lambda()(api-for-type-op iam-doc 'projects.notfound.get)))

(check-equal? (api-request-type (api-for-type-op compute-doc 'compute.networks.insert)) "ReqBody")
(check-equal? (api-response-type (api-for-type-op compute-doc 'compute.networks.insert)) "RespBody")


(test-case
 "API resource shape"
 (define req-body #hasheq((attr1 . "myvalue") (attr2 . "myvalue2") (attr3 . "myvalue3")))
 (define api (api-for-type-op compute-doc 'compute.networks.insert))
 ;  (check-equal? (api-schema (disc-api-root api) "ReqBody") #hasheq((attr1 . value)))
 (define shaped (api-resource api req-body))
 (check-equal? shaped #hasheq((attr1 . "myvalue")(attr2 . "myvalue2"))))

(test-case
 "API resource shape when no request object"
 (define req-body #hasheq((attr1 . "myvalue") (attr2 . "myvalue2") (attr3 . "myvalue3")))
 (define api (api-for-type-op compute-doc 'compute.networks.insert))
 ;  (check-equal? (api-schema (disc-api-root api) "ReqBody") #hasheq((attr1 . value)))
 (define shaped (api-resource api req-body))
 (check-equal? shaped req-body))
