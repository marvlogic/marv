#lang racket/base

(require marv/utils/hash)
(require marv/drivers/gcp/discovery)

(provide compute-api-operation-handler)

(struct op-status (id done? errors poll-next response) #:transparent)
(define (op-success resp) (op-status #f #t #f #f (lambda(_)resp)))

(define (op-success? op) (and (op-status-done? op) (not (op-status-errors op))))
(define (op-failed? op) (and (op-status-done? op) (op-status-errors op)))

(define (compute-api-operation-handler response-type resp)
  (define is-operation? (equal? "Operation" response-type))
  (cond
    [is-operation?
     (define done? (equal? "DONE" (hash-ref resp 'status)))
     (define errors (hash-nref resp '(error items) #f))
     (define poll-next (and (not done?) (lambda(http) (http 'GET (hash-ref resp 'selfLink)))))
     (define completed (and done?
                            (not errors)
                            (lambda(http) (http 'GET (hash-ref resp 'targetLink) '()))))
     (op-status (hash-ref resp 'selfLink) done?  errors poll-next completed)]
    [else (op-success resp)]))

(define (generic-req api resource http is-delete? api-operation-handler)
  (define (work-it op-resp)
    (cond
      [(op-success? op-resp)
       (displayln "..DONE\033[K")
       (flush-output)
       ((op-status-response op-resp) http)]
      [(op-failed? op-resp) (raise (format "Operation failed with errors: ~v" (op-status-errors op-resp)))]
      [(procedure? (op-status-poll-next op-resp))
       (printf "\033[s..IN PROGRESS..~a\033[u" 123)
       (flush-output)
       (sleep 5)
       ((op-status-poll-next op-resp) http)]
      [else (raise (format "indeterminate op state: ~v" op-resp))]
      ))

  (define op-resp
    (api-operation-handler (api-response-type api)
                           (http (api-http-method api)
                                 (api-resource-url api resource)
                                 (api-resource api resource))))
  (work-it op-resp))