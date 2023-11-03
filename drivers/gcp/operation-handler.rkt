#lang racket/base

(require racket/contract)
(require marv/utils/hash)

(provide compute-api-operation-handler
         iam-api-operation-handler
         (struct-out op-status)
         op-status-flag)

(struct op-status (done? errors poll-next final-response) #:prefab)
(define (op-success resp) (op-status #t #f #f (lambda(_)resp)))

(define (op-status-flag op)
  (define op-success? (and (op-status-done? op) (not (op-status-errors op))))
  (define op-failed? (and (op-status-done? op) (op-status-errors op)))
  (cond
    [op-success? 'success]
    [op-failed? 'errored]
    [else 'running]))

(define (compute-api-operation-handler response-type resp)
  (define is-operation? (equal? "Operation" response-type))
  (cond
    [is-operation?
     (define done? (equal? "DONE" (hash-ref resp 'status)))
     (define errors (hash-nref resp '(error items) #f))
     (define poll-next
       (and (not done?)
            (lambda(http)
              (compute-api-operation-handler
               "Operation"
               (http 'GET (hash-ref resp 'selfLink) '())))))
     (define completed (and done?
                            (not errors)
                            (lambda(http) (http 'GET (hash-ref resp 'targetLink) '()))))
     (op-status done?  errors poll-next completed)]
    [else (op-success resp)]))

; TODO - same code?
(define (iam-api-operation-handler response-type resp)
  (define is-operation? (equal? "Operation" response-type))
  (cond
    [is-operation?
     (define done? (equal? "DONE" (hash-ref resp 'status)))
     (define errors (hash-nref resp '(error items) #f))
     (define poll-next
       (and (not done?)
            (lambda(http)
              (compute-api-operation-handler
               "Operation"
               (http 'GET (hash-ref resp 'selfLink) '())))))
     (define completed (and done?
                            (not errors)
                            (lambda(http) (http 'GET (hash-ref resp 'targetLink) '()))))
     (op-status done?  errors poll-next completed)]
    [else (op-success resp)]))
