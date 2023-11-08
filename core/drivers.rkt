#lang racket/base

(require racket/contract)
(require racket/list)
(require racket/string)
(require marv/log)
(require marv/core/globals)
(require marv/drivers/types)
(require marv/drivers/dev)
(require marv/drivers/gcp/api)
(require marv/drivers/gcp/transformers)

(provide register-type
         current-driver-set
         current-driver
         with-drivers)

(define/contract (current-driver)
  (-> driver/c)
  (make-driver-for-set (DRIVERS)))

(define/contract (current-driver-set)
  (-> driver-set/c)
  (DRIVERS))

(define/contract (with-drivers dset func)
  (driver-set/c procedure? . -> . any/c)
  (parameterize ([DRIVERS dset]) (func)))

(define/contract (make-driver-for-set drivers)
  (driver-set/c . -> . driver/c)

  (define/contract (crudfn-for driver-id op msg)
    (driver-id/c msg-id/c any/c . -> . any/c)
    (define crud (hash-ref drivers driver-id))
    (crud op msg))
  crudfn-for)

(define (register-type driver-id type-id api-specs)

  (define type (string->symbol(string-join (map symbol->string type-id) ".")))
  (define (check-for m)
    (unless (hash-has-key? api-specs m)
      (raise (format "~a:~a does not have the required '~a' clause" driver-id type m))))
  (check-for 'create)
  ; (check-for 'read)
  (check-for 'delete)

  (log-marv-info "Registering type: ~a:~a ~a" driver-id type api-specs)

  (define (type-transform spec)
    (cond [(null? spec) (transformer null null null)]
          [else (transformer (join-symbols (first spec)) (second spec) (third spec))]))

  (define (register-type-msg type transforms) (hash '$type type 'transforms transforms))

  ((current-driver) driver-id 'register-type
                    (register-type-msg
                     type
                     (for/list ([op '(create read update delete)])
                       (type-transform (hash-ref api-specs op null)))))
  ; NB return void or the empty hash gets printed!
  (void))

(define (std-drivers)
  (if
   (getenv "MARV_DEV_DRIVER")
   (hash
    'dev (init-dev-driver 'dev2)
    'gcp (init-dev-driver 'dev))
   (hash
    'dev (init-dev-driver 'dev)
    'gcp (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))))))

(define DRIVERS (make-parameter (std-drivers)))