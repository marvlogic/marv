#lang racket/base

(require racket/contract)
(require marv/log)
(require marv/core/config)
(require marv/drivers/utils)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/generic-request-handler)
(require marv/drivers/gcp/transformers)

(provide init-api)

(define TYPE-MAP (make-parameter (hash)))

(define (init-api interface-id api-id http OPERATION-HANDLER [type-op-patches (hash)])
  (define discovery (load-discovery (symbol->string interface-id) api-id type-op-patches))
  (define (type-map-fn t)
    (hash-ref (TYPE-MAP) t (lambda()(raise-argument-error 'type "registered type" t))))
  (define (genrq cf)
    (lambda(res) ((mk-request-handler discovery type-map-fn OPERATION-HANDLER) cf res http)))

  (define crudfn
    (make-driver-crud-fn
     (lambda(cfg) (validate discovery type-map-fn cfg))
     (genrq crud-create) (genrq crud-read) (genrq crud-update) (genrq crud-delete)
     aux-handler))
  crudfn)

(define/contract (validate discovery type-map-fn cfg)
  (disc-doc? procedure? config/c   . -> . config/c)
  ; (define type-map (TYPE-MAP))
  (define type (gcp-type cfg))
  (define api (api-for-type-op discovery (crud-create(type-map-fn type))))

  ; TODO25 - check can't pass unless api-transformer has been applied...
  (define (has-required-api-parameters?)
    (define req-params (api-required-params api))
    (for/first ([p req-params] #:unless (hash-has-key? cfg p))
      (raise (format "Config does not have required field(s) (~a) ~a" p req-params))))
  ;   (has-required-api-parameters?)
  cfg)

(define (aux-handler op msg)
  (case op
    ['register-type handle-register-type]
    [else (raise "Unsupported op/message in storage-api")]))

(define (handle-register-type msg)
  (define-values (type transformers) (values (hash-ref msg '$type) (hash-ref msg 'transforms)))
  (log-marv-info "handle-register-type ~a:~a" type transformers)
  (define apis (map transformer-api-id transformers))
  (define-values (create-api read-api update-api delete-api) (apply values apis))
  (register-type type (crud create-api read-api update-api delete-api))

  ; TODO23 - tidy this
  (define tfns (map transformer-req-fn transformers))
  (define rfns (map transformer-resp-fn transformers))
  (for ([a apis]
        [req tfns]
        [resp rfns]
        #:when (procedure? req))
    (register-transformers (transformer a req resp)))
  (hash))

(define/contract (register-type type crud)
  (symbol? crud? . -> . void)
  (log-marv-info "Registering type: ~a ~a" type crud)
  (TYPE-MAP (hash-set (TYPE-MAP) type crud)))