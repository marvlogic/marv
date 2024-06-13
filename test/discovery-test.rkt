#lang racket/base

(require rackunit)
(require racket/match)
(require marv/core/globals)
(require racket/string)
(require json)
(require/expose
 marv/drivers/gcp/discovery
 (disc-doc
  disc-doc-root
  disc-api disc-api?
  api-request-type
  api-response-type
  api-resource-url
  api-resource
  api-schema
  api-for-type-op))

'discovery-test

(define (mk-disc rs)
  (define (api-id i) (symbol->string (join-symbols (list rs i))))
  (define (res-acc racc)
    (hash 'resources
          (match racc
            [(list r)
             (hash r (hash
                      'methods (hash
                                'insert
                                (hash 'id (api-id 'insert)
                                      'request #hasheq(($ref . "ReqBody"))
                                      'response #hasheq(($ref . "RespBody"))
                                      'parameterOrder '("project" "region")
                                      'parameters
                                      #hasheq(
                                       (project . #hasheq((required . #t)(location . "path")))
                                       (region . #hasheq((required . #t)(location . "path"))))
                                      'path "project/{project}/region/{region}")
                                'get
                                (hash 'id (api-id 'get)
                                      'parameterOrder '("project" "region" "aliasedName")
                                      'parameters
                                      #hasheq(
                                       (project . #hasheq((required . #t)(location . "path")))
                                       (region . #hasheq((required . #t)(location . "path")))
                                       (aliasedName . #hasheq((required . #t)(location . "path"))))
                                      'path "project/{project}/region/{region}/{aliasedName}"))))]
            [(list r rs ...)
             (hash r (res-acc rs))])))
  (hash-set* (res-acc (cdr (split-symbol rs)))
             'rootUrl "http://localhost/"
             'servicePath "v1/"
             'schemas
             #hasheq((ReqBody
                      . #hasheq((properties . #hasheq((attr1 . value)(attr2 . value))))))))

(define compute-doc (disc-doc (mk-disc 'compute.networks) (hash)))
(define iam-doc (disc-doc (mk-disc 'iam.projects.serviceAccounts.keys) (hash)))

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
 (define api (api-for-type-op compute-doc 'compute.networks.get))
 ;  (check-equal? (api-schema (disc-api-root api) "ReqBody") #hasheq((attr1 . value)))
 (define shaped (api-resource api req-body))
 (check-equal? shaped (hash)))

(test-case
 "Resource URL on insert"
 (define api (api-for-type-op compute-doc 'compute.networks.insert))
 (check-equal?
  (api-resource-url api
                    #hasheq((name . "myresource")
                            (project . "myproject")
                            (region . "europe-west1")))
  "http://localhost/v1/project/myproject/region/europe-west1"))

(test-case
 "Resource URL on get"
 (define api (api-for-type-op compute-doc 'compute.networks.get))
 (check-equal?
  (api-resource-url api
                    #hasheq((name . "myresource")
                            (project . "myproject")
                            (region . "europe-west1")))
  "http://localhost/v1/project/myproject/region/europe-west1/myresource"))

(test-case
 "Resource URL on get, missing fields in config"
 (define api (api-for-type-op compute-doc 'compute.networks.get))
 (check-exn
  exn:fail?
  (lambda()(api-resource-url api #hasheq((region . "europe-west1"))))))