#lang racket/base

(require racket/contract)
(require marv/utils/hash)
(require marv/drivers/gcp/operation-handler)
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

(define/contract (mk-request-handler op-handler-fn)
  (procedure? . -> . procedure?)

  (define/contract (request-handler cmd http)
    (driver-cmd/c any/c . -> . driver-resp/c)

    (define api (driver-spec-api cmd))
    (define config (hash-ref cmd 'config))
    (log-marv-debug "generic-request-handler: type-op=~a ~a" api config)
    (cond
      ; TODO - hacked, if null operation then we don't do anything
      [(null? api) config]
      [(hash? api)
       (define response (do-api-request api config http op-handler-fn))
       (log-marv-debug "response: ~a" response)
       response]
      [else raise (format "type has no usable CRUD for ~a : ~a" 'bah api )]
      ))
  request-handler)

(define/contract (do-api-request api resource http operation-handler)
  ; TODO-define API type/contracts
  (hash? hash? any/c procedure? . -> . hash?)

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
  ; TODO41
  (define api-shaped-resource (api-request-body api))
  (log-marv-debug "api-shaped: ~v" api-shaped-resource)
  (define initial-response
    (operation-handler (api-response-type api)
                       (http (api-http-method api)
                             (api-url api resource)
                             api-shaped-resource)))
  (work-it initial-response))