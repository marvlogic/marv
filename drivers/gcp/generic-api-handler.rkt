#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/operation-handler)
(require marv/drivers/gcp/discovery)

(provide generic-api-req)

(define (running-output counter)
  (printf "\033[s..IN PROGRESS..~a\033[u" counter)
  (flush-output))

(define (success-output)
  (displayln "..DONE\033[K")
  (flush-output))

(define/contract (generic-api-req api resource http is-delete? api-operation-handler)
  (disc-api? any/c any/c boolean? procedure? . -> . hash?)
  (define (work-it op-resp [poll-delay 3])
    (case (op-status-flag op-resp)
      ['success (define fin ((op-status-final-response op-resp) http))
                (success-output)
                fin]
      ['running (running-output poll-delay)
                (sleep poll-delay)
                (work-it ((op-status-poll-next op-resp) http) (add1 poll-delay))]
      ['errored (raise (format "Operation failed with errors: ~v" (op-status-errors op-resp)))]
      [else (raise (format "indeterminate op state: ~v" op-resp))]
      ))

  (define initial-response
    (api-operation-handler (api-response-type api)
                           (http (api-http-method api)
                                 (api-resource-url api resource)
                                 (api-resource api resource))))
  (work-it initial-response))