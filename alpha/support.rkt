#lang racket/base

(require racket/hash)
(require racket/string)
(require racket/pretty)
(require marv/log)
(require marv/core/values)
(require marv/utils/hash)
(require (prefix-in core: marv/core/resources))

(provide gen-resources gen-drivers getenv-or-raise
         def-res
         set-return
         module-call
         config-overlay config-reduce hash-nref handle-ref
         with-module-ctx get-param
         register-type)

(define (error:excn msg)
  (raise (format "ERROR at ~a:~a :  ~a" 1 2 msg))) ;(syntax-source stx) (syntax-line stx)))

(define (init-resources) (list null (hash)))
(define (add-resource id res)
  (define idx (car (RESOURCES)))
  (define hs (cadr (RESOURCES)))
  (when (hash-has-key? hs id) (error:excn (format "~a is already defined" id)))
  (RESOURCES (list (cons id idx) (hash-set hs id res)))
  res)
(define (ordered-resource-ids) (reverse (car (RESOURCES))))
(define (get-resource id) (hash-ref (cadr (RESOURCES)) id))

(define RESOURCES (make-parameter (init-resources)))

(define MODULE-PREFIX (make-parameter #f))
(define (prefix-mod-id i) (core:prefix-id (MODULE-PREFIX) i))
(define (with-module-ctx id-prefix params proc)
  (log-marv-debug "Switching into module-context ~a" id-prefix)
  (parameterize ([PARAMS params]
                 [RESOURCES (init-resources)]
                 [MODULE-PREFIX id-prefix ])
    (proc)))

(define PARAMS (make-parameter (hash)))
(define (get-param p [def (lambda()
                            (error:excn (format "Parameter '~a' has not been assigned" p)))])
  (hash-ref (PARAMS) p def))

(define (def-res id drv attr v)
  (define r (hash-set* v '$driver drv '$type (string->symbol (string-join (map symbol->string attr) "."))))
  (add-resource id r))

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

(define (try-resolve-future-ref id #:fail-on-missing (fail-on-missing #f))
  (define fail (if fail-on-missing
                   (lambda() (log-marv-error "future-ref not found: ~a~n  in ~a" id (RETURNS))
                     (error:excn "future-ref not found"))
                   (lambda() (log-marv-warn "future-ref not yet resolved (~a)" id)(future-ref id))))
  (hash-ref (RETURNS) id fail))

(define (resource? res) (and (hash? res) (hash-has-key? res '$driver)))

(struct future-ref (ref) #:prefab)

(define (module-call var-id mod-proc params)
  (log-marv-debug "Registering future invocation of ~a=~a(~a)" (prefix-mod-id var-id) mod-proc params)
  (add-resource var-id (lambda(id-prefix mkres)
                         (log-marv-debug "calling ~a (~a)" var-id mod-proc)
                         (mod-proc (prefix-mod-id var-id) mkres params)))
  (future-ref #f))

(define (config-overlay left right) (hash-union left right #:combine (lambda (v0 _) v0)))
(define (config-reduce cfg attrs) (hash-take cfg attrs))

(define (make-full-ref full-id attrs)
  (string->symbol (format "~a/~a" full-id attrs)))

(define (handle-ref tgt id . ks)
  (log-marv-debug "handle-ref ~a.~a -> ~a" id ks tgt)
  (define ksx (map syntax-e ks))
  (define (full-ref) (make-full-ref (prefix-mod-id id) (core:list->id ksx)))

  (cond [(resource? tgt) (ref (full-ref))]
        [(hash? tgt) (hash-nref tgt ksx)]
        [(future-ref? tgt)
         (define resolved (try-resolve-future-ref (full-ref)))
         (log-marv-debug "(future-ref, being re-linked to ~a)" resolved)
         resolved]
        [else (raise "unsupported ref type")]))

(define (gen-resources mkres)
  (log-marv-debug "gen-resources called for these visible resources: ~a" (RESOURCES))

  (define (handle-future-ref k v)
    (update-val v (lambda(uv)
                    (if (future-ref? uv)
                        (try-resolve-future-ref (future-ref-ref uv) #:fail-on-missing #t)
                        uv))))

  (define (make-resource v)
    (log-marv-debug "  generating ~a" v)
    (mkres (hash-ref v '$driver) (hash-apply (hash-remove v '$driver) handle-future-ref)))

  (for/fold ([rs (hash)])
            ([k (in-list (ordered-resource-ids))])
    (define res (get-resource k))
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

(define (tmp-drivers)
  (if
   (getenv "MARV_DEV_DRIVER")
   (hash
    'dev (init-dev-driver 'dev2)
    'gcp (init-dev-driver 'dev))
   (hash
    'dev (init-dev-driver 'dev)
    'gcp (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))))))
