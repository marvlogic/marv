#lang racket/base

(require racket/hash)
(require racket/format)
(require racket/match)
(require racket/set)
(require marv/log)
(require marv/core/values)
(require marv/utils/hash)
(require marv/utils/uri)
(require marv/core/globals)
(require marv/core/drivers)
(require marv/utils/base64)
(require marv/core/resources)
(require marv/core/modules)


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
         config-overlay config-reduce
         dot-op
         check-for-ref
         resolve-terms
         add-dep get-deps
         with-module-ctx
         with-resource-prefix
         get-resource-prefix
         get-resources
         ordered-resource-ids
         uri-vars
         uri-template
         find-function
         get-param
         src-location
         raise/src
         check-operator-types)

(define (error:excn msg)
  (raise (format "ERROR at ~a:~a :  ~a" 1 2 msg))) ;(syntax-source stx) (syntax-line stx)))

; TODO45 - simplification of resource assembly probably means code to manage
; namespace stuff isn't needed.

(define (init-resources) (list null (hash)))
(define RESOURCES (make-parameter (init-resources)))
(define (add-resource id res)
  (define idx (car (RESOURCES)))
  (define hs (cadr (RESOURCES)))
  (when (hash-has-key? hs id) (error:excn (format "~a is already defined" id)))
  (RESOURCES (list (cons id idx) (hash-set hs id res)))
  res)
(define (ordered-resource-ids) (reverse (car (RESOURCES))))
(define (get-resource id) (hash-ref (cadr (RESOURCES)) id))
(define (get-resources) (cadr (RESOURCES)))

(define MODULE-PREFIX (make-parameter 'main))

(define init-var-deps set)
(define VAR-DEPS (make-parameter (init-var-deps)))
(define (add-dep d) (VAR-DEPS (set-add (VAR-DEPS) d)))
(define (get-deps) (set->list(VAR-DEPS)))

(define (prefix-mod-id i) (core:prefix-id (MODULE-PREFIX) i))

; TODO45 - module calls are simplified?

(define (with-module-ctx params proc)
  (log-marv-debug "Switching into module-context: ~a" (MODULE-PREFIX))
  (parameterize ([PARAMS params]
                 )
    ;  [RESOURCES (init-resources)])
    (proc)))

(define (with-resource-prefix id proc)
  (define new-prefix (join-symbols (list (MODULE-PREFIX) id)))
  (log-marv-debug "Setting resource prefix: ~a" new-prefix)
  (parameterize
      ([MODULE-PREFIX new-prefix]
       [VAR-DEPS (init-var-deps)])
    (define r (proc))
    (log-marv-debug "~a depends on ~a" id (get-deps))
    r))

(define (get-resource-prefix) (MODULE-PREFIX))

(define PARAMS (make-parameter (hash)))
(define (get-param p [def (lambda()
                            (error:excn (format "Parameter '~a' has not been assigned" p)))])
  (hash-ref (PARAMS) p def))

(define type-id-key 'type-id)

(define (def-res type-id id res)
  ; Temporarily storing ref to the type-fn in the configuration, it will
  ; be removed later when creating a resource
  ; (define rtyped (hash-set res type-id-key type-id))
  (define gid (join-symbols (list (get-resource-prefix) id)))
  (log-marv-debug "Defining resource: ~a" gid)
  (add-resource gid (resource gid type-id (get-deps) res)))

(define (with-src-handlers src-locn expected given thunk)
  (define (handle-exn e)
    (raise-argument-error
     'type
     (format "~a at ~a~nActual exception:~n~a~n)" expected src-locn e) given))
  (with-handlers ([exn? handle-exn]) (thunk)))

; TODO45 module returns simpler
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

; TODO45 - review if future-refs are still needed?
(define (try-resolve-future-ref id #:fail-on-missing (fail-on-missing #f))
  (define fail (if fail-on-missing
                   (lambda() (log-marv-error "future-ref not found: ~a~n  in ~a" id (RETURNS))
                     (error:excn "future-ref not found"))
                   (lambda() (log-marv-warn "future-ref not yet resolved (~a)" id)(future-ref id))))
  (hash-ref (RETURNS) id fail))


(struct future-ref (ref) #:prefab)

; TODO45 - is this still needed?
(define (module-call var-id mod-proc params)
  (log-marv-debug "Registering future invocation of ~a=~a(~a)" (prefix-mod-id var-id) mod-proc params)
  (add-resource var-id (lambda(_)
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

(define STATE (make-parameter (hash)))

(define (dot-op tgt attr)
  (log-marv-debug "dot-op: ~a . ~a" tgt attr)
  (define r
    (cond
      [(resource? tgt) (ref (resource-gid tgt) attr)]
      [(hash? tgt) (hash-ref tgt attr)]
      [(future-ref? tgt)
       (define resolved (try-resolve-future-ref attr))
       (log-marv-debug "(future-ref, being re-linked to ~a)" resolved)
       resolved]
      [else (raise "unsupported ref type")]))
  (when (ref? r) (add-dep r))
  r)

(define (check-for-ref term)
  (log-marv-debug "-> checking ref: ~a" term)
  (when (ref? term) (add-dep term))
  term)

(struct deferred (op term1 term2) #:prefab)

(define (resolve-terms op term1 term2)

  (log-marv-debug "Resolving: ~a:~a:~a" op term1 term2)
  (define (try-resolve e)

    (define (resolve-ref r)
      (log-marv-debug "-> attempting to resolve: ~a" r)
      (hash-nref
       (resource-config(hash-ref (get-resources) (ref-gid r)))
       (id->list (ref-path r))
       r))

    (if (ref? e) (resolve-ref e) e))

  (define t1 (try-resolve term1))
  (define t2 (try-resolve term2))
  (log-marv-debug "-> t1: ~a t2: ~a" t1 t2)
  (cond [(or (ref? t1) (ref? t2) (deferred? t1) (deferred? t2))
         (log-marv-debug "-> couldn't resolve, deferred (t1:~a t2:~a)" t1 t2)
         (deferred op t1 t2)]
        [else
         (log-marv-debug "-> resolved, invoking operation: ~a" op)
         (define r (op t1 t2))
         (log-marv-debug "<- finished resolving: ~a" r)
         r ]
        ))

; TODO45 - not sure if this step is needed, can resources be defined in-line?
(define (gen-resources)
  (log-marv-debug "gen-resources called for these visible resources: ~a" (ordered-resource-ids))
  (log-marv-debug "-> ~a" (RESOURCES))

  (define (handle-future-ref _ v)
    (update-val v (lambda(uv)
                    (if (future-ref? uv)
                        (try-resolve-future-ref (future-ref-ref uv) #:fail-on-missing #t)
                        uv))))

  (define (make-resource res)
    (log-marv-debug "  generating ~a" res)
    (define res-ident (resource-call 'identity res))
    (log-marv-debug "  identity ~a" res-ident)
    (resource (resource-gid res) (resource-type res) (hash-apply res-ident handle-future-ref)))

  (for/fold ([rs (hash)])
            ([k (in-list (ordered-resource-ids))])
    (define res (get-resource k))
    (log-marv-debug "looking at ~a"  k )
    (cond [(resource? res) (hash-set rs (resource-gid res) (make-resource res))]
          ; [(hash? res) (hash-set rs (prefix-mod-id k) (make-resource res))]
          [else (raise "unsupported reference/resource stuff")])))
; [(procedure? res)
;  ; Calling a module
;  (hash-union rs (res (prefix-mod-id k)))])))

(define (uri-vars str) (uri-vars str))
(define (uri-template str cfg) (expand-uri str cfg))

(define (find-function root fst rst)
  (log-marv-debug "find-function: ~a.~a" fst rst)
  (define func
    (match/values
     (values root rst)
     [((? procedure? proc) (? null? _) )
      (log-marv-debug "  ->func: ~a" proc)proc]
     [((? procedure? proc) (list r))
      (log-marv-debug "  ->func ~a in type ~a" r proc)
      (proc r)]
     [((? hash? hsh) lst)
      (log-marv-debug "  ->func in hash")
      (hash-nref hsh lst)]
     [(_ _) (raise "unsupported function reference")]))
  (if (procedure? func) func (raise (~a "expected a function, got: " func))))

(define (src-location s) (format "~a:~a" (syntax-source s) (syntax-line s)))

(define (raise/src locn msg) (raise (~a "Error:" msg " at " locn) ))

(define (check-operator-types locn test1? test2? term1 term2)
  ;TODO45 - check that indirection isn't needed, term1 & 2 should have been evaluated by now
  (unless (and (test1? term1) (test2? term2)) (raise (~a "terms not valid at:" locn "~n got " term1 "~n and " term2))))