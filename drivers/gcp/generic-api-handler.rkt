#lang racket/base

(require racket/contract)
(require marv/utils/hash)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/transformers)
(require marv/drivers/gcp/crud)
(require marv/log)
(require marv/core/config)

(provide mk-request-handler)

(define (running-output counter)
  (printf "\033[s..IN PROGRESS..~a\033[u" counter)
  (flush-output))

(define (success-output)
  (displayln "..DONE\033[K")
  (flush-output))

(define (mk-request-handler discovery-doc type-map op-handler)

  (define (gcp-type r) (hash-ref r '$type))

  (define/contract (generic-request crud-fn config http)
    (procedure? config/c any/c . -> . config/c)
    (define type-op (crud-fn (type-map (gcp-type config))))
    (log-marv-debug "gen/req: type-op=~a ~a" type-op config)
    (define xfd-resource (apply-request-transformer type-op config) )
    (log-marv-debug "xformed: ~a" xfd-resource)
    (cond
      ; TODO - hacked
      [(null? type-op) xfd-resource]
      [(symbol? type-op)
       (define api (api-for-type-op discovery-doc type-op))
       (define is-delete? (eq? crud-delete crud-fn))
       (define response (generic-api-req api xfd-resource http is-delete? op-handler))
       (log-marv-debug "response: ~a" response)
       ;  (define resp (api-resource api response))
       (hash-merge response config)]
      [else raise (format "type has no usable CRUD for ~a : ~a" crud-fn type-op )]
      ))
  generic-request)

(define/contract (generic-api-req api resource http is-delete? operation-handler)
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

  ; TODO14 - hash-dropping the project/region fields?
  (define initial-response
    (operation-handler (api-response-type api)
                       (http (api-http-method api)
                             (api-resource-url api resource)
                             (api-resource api (hash-drop resource '(project region))))))
  (work-it initial-response))