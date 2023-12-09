#lang racket/base

(require racket/contract)
(require racket/match)
(require marv/log)
(require marv/core/config)
(require marv/drivers/utils)
(require marv/drivers/gcp/crud)
(require marv/drivers/gcp/discovery)
(require marv/drivers/gcp/generic-request-handler)
(require marv/drivers/gcp/transformers)
(require marv/core/globals)

(provide init-api)

(define (init-api interface-id api-id http OPERATION-HANDLER [type-op-patches (hash)])
  (define discovery (load-discovery (symbol->string interface-id) api-id type-op-patches))
  (mk-request-handler discovery OPERATION-HANDLER))

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

(define TYPE-MAP (make-parameter #f))
(define (handle-show-docs discovery msg)
  (define type-and-sub (split-symbol (gcp-type msg) "/"))
  (define type (car type-and-sub))
  (define subtype (if (eq? 1 (length type-and-sub)) #f (cadr type-and-sub)))

  (define real-type (hash-ref (TYPE-MAP) type))
  (define type-op (crud-create real-type))
  (define api (api-for-type-op discovery type-op))
  (api-display-docs api (api-request-type api) subtype)
  (hash))
