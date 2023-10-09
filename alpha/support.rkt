#lang racket/base

(require racket/hash)
(require racket/string)
(require racket/pretty)
(require marv/log)
(require marv/core/values)
(require marv/utils/hash)
(require (prefix-in core: marv/core/resources))

(provide gen-resources gen-drivers getenv-or-raise
         hash-union
         set-var
         set-return
         get-var
         def-res
         module-call
         resource-var? config-overlay hash-nref handle-ref
         with-module-ctx get-param
         register-type)

(define (error:excn msg)
  (raise (format "ERROR: ~a\n at ~a:~a" msg 1 2))) ;(syntax-source stx) (syntax-line stx)))

(define PARAMS (make-parameter (hash)))
(define MODULE-PREFIX (make-parameter #f))
(define VARS (make-parameter (hash)))

(define (prefix-mod-id i) (core:prefix-id (MODULE-PREFIX) i))

(define (with-module-ctx id-prefix params proc)
  (log-marv-debug "Switching into module-context ~a" id-prefix)
  (parameterize ([PARAMS params]
                 [VARS (hash)]
                 [MODULE-PREFIX id-prefix ])
    (proc)))

(define (get-param p) (hash-ref (PARAMS) p))

(define (set-var id v)
  (when (hash-has-key? (VARS) id) (error:excn (format "~a is already defined" id)))
  (VARS (hash-set (VARS) id v)))
(define (get-var id) (hash-ref (VARS) id))

(define RETURNS (make-parameter (hash)))
(define (set-return v)
  (log-marv-debug "** CURRENT RETURNS: ~a" (RETURNS))
  (define id (MODULE-PREFIX))
  (RETURNS (for/fold ([hs (RETURNS)])
                     ([k (in-list (hash-keys v))])
             (define new-id (make-full-ref id k))
             (define val (hash-ref v k))
             (log-marv-debug "Setting return value: ~a -> ~a" new-id val)
             (hash-set hs new-id val)
             )))

(define (try-resolve-future-ref id)
  (hash-ref (RETURNS) id (lambda() (future-ref id))))

(define (def-res id drv attr v)
  (define r (hash-set* v '$driver drv '$type (string->symbol (string-join (map symbol->string attr) "."))))
  (set-var id r)
  r)

(define (resource? res) (and (hash? res) (hash-has-key? res '$driver)))
(define (resource-var? id) (resource? (hash-ref (VARS) id)))

(struct future-ref (ref) #:prefab)

(define (module-call var-id mod-proc params)
  (log-marv-debug "Registering future invocation of ~a=~a(~a)" (prefix-mod-id var-id) mod-proc params)
  (set-var var-id (lambda(id-prefix mkres) (mod-proc (prefix-mod-id var-id) mkres params)))
  (future-ref #f))

(define (config-overlay left right) (hash-union left right #:combine (lambda (v0 _) v0)))
; (define (config-take cfg attrs) (hash-take cfg attrs))

(define (make-full-ref full-id attrs)
  (string->symbol (format "~a/~a" full-id attrs)))

(define (handle-ref tgt id . ks)
  (define ksx (map syntax-e ks))
  (define full-ref (make-full-ref (prefix-mod-id id) (core:list->id ksx)))
  (log-marv-debug "Handle ref: ~a ~a (full-ref=~a)" id ksx full-ref)

  (cond [(resource? tgt) (ref full-ref)]
        [(hash? tgt) (hash-nref tgt ksx)]
        [(future-ref? tgt)
         (define resolved (try-resolve-future-ref full-ref))
         (log-marv-debug "(future-ref, being re-linked to ~a)" resolved)
         resolved]
        [else (raise "unsupported ref type")]))

(define (gen-resources mkres)
  (log-marv-debug "gen-resources for: ~a" (VARS))

  (define (handle-future-ref k v)
    ; TODO - what if not found?
    (if (future-ref? v)
        (try-resolve-future-ref (future-ref-ref v))
        v))

  (define (make-resource v)
    (mkres (hash-ref v '$driver)
           (hash-apply (hash-remove v '$driver)
                       handle-future-ref)))

  (for/fold ([rs (hash)])
            ([k (hash-keys (VARS))])
    (define res (hash-ref (VARS) k))
    (cond [(resource? res)
           (hash-set rs (prefix-mod-id k) (make-resource res))]
          [(procedure? res)
           (hash-union rs (res (prefix-mod-id k) mkres))])))

(define (gen-drivers decls-hs) tmp-drivers)

(define (getenv-or-raise e)
  (or (getenv e) (raise (format "ERROR: ~a must be defined in environment" e))))

(require marv/drivers/driver)
(require marv/drivers/dev)
(require marv/drivers/gcp/api)
(require marv/drivers/gcp/transformers)

(define (register-type driver-id type-id api-specs)

  (define type (string->symbol(string-join (map symbol->string type-id) ".")))
  (define (check-for m)
    (unless (hash-has-key? api-specs m)
      (raise (format "~a:~a does not have the required '~a' clause" driver-id type m))))
  (check-for 'create)
  (check-for 'delete)

  (log-marv-info "Registering: ~a:~a ~a" driver-id type api-specs)

  (define (type-transform spec)
    (cond [(null? spec) (transformer null null)]
          [else (transformer (string->symbol(string-join (map symbol->string (car spec)) ".")) (cadr spec))]))

  (define (register-type-msg type transforms) (hash '$type type 'transforms transforms))

  (define driver-head (make-driver-for-set (tmp-drivers)))
  (driver-head driver-id 'register-type
               (register-type-msg
                type
                (for/list ([op '(create read update delete)])
                  (type-transform (hash-ref api-specs op null)))))
  ; NB return void or the empty hash gets printed!
  (void))

(define defaults
  (hash 'project (getenv-or-raise "MARV_GCP_PROJECT")
        'region  (getenv-or-raise "MARV_GCP_REGION")))

(define (tmp-drivers)
  (hash
   'dev (init-dev-driver 'dev2)
   'gcp (init-dev-driver 'dev)
   'gcp2 (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))
                   )))
