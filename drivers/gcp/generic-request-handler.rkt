#lang racket/base

(require racket/contract)
(require marv/utils/hash)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/transformers)
(require marv/drivers/gcp/crud)
(require marv/drivers/utils)
(require marv/log)
(require marv/core/config)

(provide mk-request-handler)

(define (running-output counter)
  (printf "\033[s..IN PROGRESS..~a\033[u" counter)
  (flush-output))

(define (success-output)
  (displayln "..DONE\033[K")
  (flush-output))

(define/contract (mk-request-handler discovery-doc type-map-fn op-handler-fn)
  (disc-doc? procedure? procedure? . -> . procedure?)

  (define/contract (request-handler crud-fn config http)
    (procedure? config/c any/c . -> . config/c)
    (define type-op (crud-fn (type-map-fn (gcp-type config))))
    (log-marv-debug "generic-request-handler: type-op=~a ~a" type-op config)
    (define xfd-resource (apply-request-transformer type-op config) )
    (log-marv-debug "transformed: ~v" xfd-resource)
    (cond
      ; TODO - hacked, if null operation then we don't do anything
      [(null? type-op) xfd-resource]
      [(symbol? type-op)
       (define api (api-for-type-op discovery-doc type-op))
       (define is-delete? (eq? crud-delete crud-fn))
       (define response (do-api-request api xfd-resource http is-delete? op-handler-fn))
       (log-marv-debug "response: ~a" response)
       (define xresp (apply-response-transformer type-op response))
       (log-marv-debug "transformed: ~a" xresp)
       (hash-merge config xresp)]
      [else raise (format "type has no usable CRUD for ~a : ~a" crud-fn type-op )]
      ))
  request-handler)

(define/contract (do-api-request api resource http is-delete? operation-handler)
  (disc-api? any/c any/c boolean? procedure? . -> . hash?)

  (define (work-it op-resp [poll-delay 3])
    (case (op-status-flag op-resp)
      ['success (define fin (if is-delete? (hash) ((op-status-final-response op-resp) http)))
                (success-output)
                fin]
      ['running (running-output poll-delay)
                (sleep poll-delay)
                (work-it ((op-status-poll-next op-resp) http) (add1 poll-delay))]
      ['errored (raise (format "Operation failed with errors: ~v" (op-status-errors op-resp)))]
      [else (raise (format "indeterminate op state: ~v" op-resp))]
      ))
  (define api-shaped-resource (api-resource api resource))
  (log-marv-debug "api-shaped: ~v" api-shaped-resource)
  (define initial-response
    (operation-handler (api-response-type api)
                       (http (api-http-method api)
                             (api-resource-url api resource)
                             api-shaped-resource)))
  (work-it initial-response))