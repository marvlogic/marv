
#lang racket/base

(require marv/drivers/gcp/transformers)
(require marv/utils/hash)
(require marv/utils/base64)

(provide request-transformers)

(define (request-transformers)

  (define (secret-parent res) (hash-set res 'parent (format "projects/~a" (hash-ref res 'project))))
  (register-request-transformer (transformer 'secretmanager.projects.secrets.create secret-parent))

  (define (secret-version res)
    (hash-set* res
               'payload (hash 'data (b64enc (hash-ref res 'secret-data)))
               'name (hash-ref res 'secret)))
  (register-request-transformer
   (transformer 'secretmanager.projects.secrets.addVersion secret-version))

  (define (secret-version-access-request res)
    (hash-set res 'name (format "~a/versions/latest" (hash-ref res 'secret))))
  (register-request-transformer
   (transformer 'secretmanager.projects.secrets.versions.access secret-version-access-request))

  (define (secret-version-access-response res)
    (hash-remove
     (hash-set res 'secret-data (b64dec(hash-nref res '(payload data))))
     'payload))

  (register-response-transformer
   (transformer 'secretmanager.projects.secrets.versions.access secret-version-access-response)))