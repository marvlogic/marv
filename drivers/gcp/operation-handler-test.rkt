#lang racket/base

(require rackunit)
(require/expose
 marv/drivers/gcp/operation-handler
 (compute-api-operation-handler
  op-status-final-response
  op-status-poll-next
  op-status-flag
  op-status?
  op-status-errors))

'operation-handler-test

(define done-resp
  #hasheq(
   (id . "65076254")
   (kind . "compute#operation")
   (name . "operation-8e180b58-25f18911")
   (operationType . "insert")
   (progress . 100)
   (selfLink . "https://completed-operation")
   (status . "DONE")
   (targetLink . "https://final-target-link-done")
   ))

(define running-resp
  #hasheq(
   (id . "65076254")
   (kind . "compute#operation")
   (name . "operation-8e180b58-25f18911")
   (operationType . "insert")
   (progress . 0)
   (selfLink . "https://running-operation")
   (status . "RUNNING")
   (targetLink . "https://final-target-link-running")
   ))

(define error-resp
  #hasheq(
   (id . "65076254")
   (kind . "compute#operation")
   (name . "operation-8e180b58-25f18911")
   (operationType . "insert")
   (progress . 0)
   (selfLink . "https://errored-operation")
   (error . #hasheq((items . ["AN ERROR" "ANOTHER"])))
   (status . "DONE")
   (targetLink . "https://final-target-link-error")
   ))

(define running (compute-api-operation-handler "Operation" running-resp))
(check-eq? (op-status-flag running) 'running)
(check-false (op-status-final-response running))
(check-false (op-status-errors running))
(check-true (procedure? (op-status-poll-next running)))
(define (mock-http-running . h)
  (check-equal? h '(GET "https://running-operation" ()))
  running-resp)
(define run-next ((op-status-poll-next running) mock-http-running))
(check-eq? (op-status-flag run-next) 'running)

(define done (compute-api-operation-handler "Operation" done-resp))
(check-eq? (op-status-flag done) 'success)
(check-false (op-status-poll-next done))
(check-false (op-status-errors done))
(check-true (procedure? (op-status-final-response done)))
(define (mock-http-done . h)
  (check-equal? h '(GET "https://final-target-link-done" ()))
  (hash))
(check-equal? ((op-status-final-response done) mock-http-done) (hash))

(define errored (compute-api-operation-handler "Operation" error-resp))
(check-eq? (op-status-flag errored) 'errored)
(check-false (op-status-poll-next errored))
(check-equal? (op-status-errors errored) '("AN ERROR" "ANOTHER"))

(define (mock-http-non-op . h) (fail-check "Shouldn't call me!"))
(define non-operation-resp (hash 'something 'else))
(check-eq? ((op-status-final-response (compute-api-operation-handler "Object" non-operation-resp))
            mock-http-non-op)
           non-operation-resp)