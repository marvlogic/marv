#lang racket/base

(require racket/hash)
(require racket/string)
(require racket/pretty)
(require racket/contract)
(require marv/log)
(require marv/core/values)
(require marv/utils/hash)
(require marv/utils/uri)
(require marv/core/globals)
(require marv/core/drivers)
(require marv/core/config)
(require marv/utils/base64)
(require marv/core/resources)
(require marv/drivers/types)


(require (prefix-in core: marv/core/resources))
(require (prefix-in drv: marv/core/drivers))

(provide gen-resources getenv-or-raise
         def-res
         with-src-handlers
         set-return
         send-to-driver
         module-call
         b64enc b64dec
         handle-override
         config-overlay config-reduce handle-ref
         with-module-ctx
         uri-vars
         uri-template
         find-function
         get-param)

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

(define (def-res type-fn id res)
  ;(type-fn 'validate res)
  ; Temporarily storing ref to the type-fn in the configuration, it will
  ; be removed later when creating a resource
  (define rtyped (hash-set res '$type-fn type-fn))
  (add-resource id rtyped))

; (define/contract (prepare-for-driver driver-spec config)
;   (driver-id/c symbol? driver-cmd/c config/c . -> . driver-resp/c)
;   (log-marv-debug "prepare-for-driver: ~a ~a" driver-spec config)
;   (define reply (hash 'config cfg 'origin (format "~a:~a" driver-id type-id)))
;   (log-marv-debug "support-send-to-driver: reply: ~a" reply)
;   reply)

(define (with-src-handlers src-locn expected given thunk)
  (define (handle-exn e)
    (raise-argument-error
     'type
     (format "~a at ~a~nActual exception:~n~a~n)" expected src-locn e) given))
  (with-handlers ([exn? handle-exn]) (thunk)))

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


(struct future-ref (ref) #:prefab)

(define (module-call var-id mod-proc params)
  (log-marv-debug "Registering future invocation of ~a=~a(~a)" (prefix-mod-id var-id) mod-proc params)
  (add-resource var-id (lambda(id-prefix)
                         (log-marv-debug "calling ~a (~a)" var-id mod-proc)
                         (mod-proc (prefix-mod-id var-id) params)))
  (future-ref #f))

(define (config-overlay top bottom) (hash-union top bottom #:combine (lambda (t _) t)))

(define (config-reduce cfg attrs) (hash-take cfg attrs))

(define (make-full-ref full-id attrs)
  (string->symbol (format "~a/~a" full-id attrs)))

(define (handle-override base op verb confex)
  (case op
    ['equals confex]
    ['overlay (config-overlay confex (base verb))]))

(define (handle-ref tgt id path)
  (log-marv-debug "handle-ref ~a.~a -> ~a" id path tgt)
  (define (full-ref) (make-full-ref (prefix-mod-id id) (core:list->id path)))

  ;TODO41- sort out dep on $type-fn
  (define (resource? res) (and (hash? res) (hash-has-key? res '$type-fn)))

  (cond [(resource? tgt) (ref (full-ref))]
        [(hash? tgt) (hash-nref tgt path)]
        [(future-ref? tgt)
         (define resolved (try-resolve-future-ref (full-ref)))
         (log-marv-debug "(future-ref, being re-linked to ~a)" resolved)
         resolved]
        [else (raise "unsupported ref type")]))

(define (gen-resources)
  (log-marv-debug "gen-resources called for these visible resources: ~a" (RESOURCES))

  (define (handle-future-ref k v)
    (update-val v (lambda(uv)
                    (if (future-ref? uv)
                        (try-resolve-future-ref (future-ref-ref uv) #:fail-on-missing #t)
                        uv))))

  (define (make-resource v)
    (log-marv-debug "  generating ~a" v)
    (define type-fn (hash-ref v '$type-fn))
    (define res-ident ((type-fn 'identity) (hash-remove v '$type-fn)))
    (log-marv-debug "  identity ~a" res-ident)
    (resource type-fn (hash-apply res-ident handle-future-ref)))

  (for/fold ([rs (hash)])
            ([k (in-list (ordered-resource-ids))])
    (define res (get-resource k))
    (cond [(hash? res)
           (hash-set rs (prefix-mod-id k) (make-resource res))]
          [(procedure? res)
           ; Calling a module
           (hash-union rs (res (prefix-mod-id k)))])))

(define (uri-vars str) (uri-vars str))
(define (uri-template str cfg) (expand-uri str cfg))

(define (find-function root fst rst)
  (log-marv-debug "find-function: ~a.~a" fst rst)
  (define func
    (cond
      [(and (procedure? root) (null? rst)) (log-marv-debug "  ->func")root]
      ; TODO41 - check rst is singleton. Also, safe to assume it's a type?
      [(and (procedure? root)) (log-marv-debug "  ->func in type")(root (car rst))]
      [(hash? root) (log-marv-debug "  ->func in hash")(hash-nref root rst)]
      [else (raise "unsupported function reference")]))
  ;TODO41 - check func is a procedure?
  func
  )