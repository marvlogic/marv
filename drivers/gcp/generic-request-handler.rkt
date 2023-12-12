#lang racket/base

(require racket/contract)
(require marv/utils/hash)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/discovery)
(require marv/drivers/types)
(require marv/log)
(require marv/core/config)

(provide mk-request-handler)

(define (running-output counter)
  (printf "\033[s..IN PROGRESS..~a\033[u" counter)
  (flush-output))

(define (success-output)
  (displayln "..DONE\033[K")
  (flush-output))

(define/contract (mk-request-handler discovery-doc op-handler-fn)
  (disc-doc? procedure? . -> . procedure?)

  (define/contract (request-handler api-spec config http)
    (driver-cmd/c config/c any/c . -> . driver-resp/c)

    (define api-id (driver-spec-api api-spec))
    ; (define pre (driver-spec-pre-fn api-spec))
    ; (define post (driver-spec-post-fn api-spec))
    (log-marv-debug "generic-request-handler: type-op=~a ~a" api-id config)
    ; (define xfd-resource (pre config) )
    ; (log-marv-debug "transformed: ~v" xfd-resource)
    ;TODO41 - this cond is not right
    (cond
      ; TODO - hacked, if null operation then we don't do anything
      [(null? api-id) config]
      [(string? api-id)
       (define api (api-for-type-op discovery-doc (string->symbol api-id)))
       ;  (define is-delete? (eq? crud-delete crud-fn))
       (define response (do-api-request api config http op-handler-fn))
       (log-marv-debug "response: ~a" response)
       ;  (define xresp (post response))
       ;  (log-marv-debug "transformed: ~a" xresp)
       response]
      [else raise (format "type has no usable CRUD for ~a : ~a" 'bah api-id )]
      ))
  request-handler)

(define/contract (do-api-request api resource http operation-handler)
  (disc-api? any/c any/c procedure? . -> . hash?)

  (define (work-it op-resp [poll-delay 3])
    (case (op-status-flag op-resp)
      ['success (define fin ((op-status-final-response op-resp) http))
                (success-output)
                (if (eof-object? fin) (hash) fin)]
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