#lang racket/base

(require racket/set)
(require racket/function)
(require racket/match)
(require racket/pretty)
(require racket/hash)
(require racket/string)
(require "state.rkt")
(require "diff.rkt")
(require "resource-def.rkt")
(require "resources.rkt")
(require "graph.rkt")
(require "hash-utils.rkt")
(require "driver.rkt")

(provide import-resources
         plan-changes
         apply-changes
         (struct-out plan))

(struct plan (resources desired-state ordered-operations) #:prefab)

(struct op-base (reason) #:transparent)
(struct op-create(op-base) #:transparent)
(struct op-replace(op-base) #:transparent)
(struct op-update(op-base) #:transparent)
(struct op-delete(op-base) #:transparent)

(define (plan-changes mod #:refresh? (refresh? #t))
  (define mdrv (make-master-driver (rmodule-drivers mod)))
  (get-plan-for mod #:refresh (mk-refresh-func (driver-read mdrv))))

(define (import-resources mod ids)
  (define md (make-master-driver (rmodule-drivers mod)))
  (define (import-one id)
    (displayln (format "importing ~a" id))
    (state-set-ref id
                   ((driver-read md)
                    (unwrap-values(deref-resource
                                   (rmodule (rmodule-drivers mod) (mk-id->state)) (resource-ref mod id))))))

  (map (lambda(id) (import-one id)) ids))

(define (mk-refresh-func readfn)

  (define (update k)
    (displayln (format "Refreshing ~a" k))
    (state-set-ref k (readfn (state-ref k))))

  (lambda (resources-meta) (map update resources-meta)))


(define (apply-changes mod #:refresh (refresh #t))

  (define mdrv (make-master-driver (rmodule-drivers mod)))

  (define (create id)
    ; (pretty-print (driver-repr id))
    (display (format "CREATING ~a" id))
    (flush-output)
    (state-set-ref id ((driver-create mdrv) (driver-repr id))))

  (define (delete id)
    (display (format "DELETING ~a" id))
    (flush-output)
    ((driver-delete mdrv) (state-ref id))
    (state-delete id))

  (define (update id)
    (display (format "UPDATING ~a" id))
    (flush-output)
    (state-set-ref id ((driver-update mdrv) (driver-repr id))))

  ; TODO - check if unpacking is done here or should use unwrap fn
  (define (driver-repr id)
    (unwrap-values
     (deref-resource (rmodule (rmodule-drivers mod) (mk-id->state))
                     (resource-ref mod id))))

  (define plan (get-plan-for mod #:refresh (mk-refresh-func (driver-read mdrv))))

  (define (delete-replacements res-op)
    (define-values (id op) (values (car res-op) (cdr res-op)))
    (if (op-replace? op) (delete id) #f))

  (define (process-op res-op)
    (match res-op
      [(cons id #f) void]
      [(cons id (op-create _)) (create id)]
      [(cons id (op-replace _)) (create id)]
      [(cons id (op-update _)) (update id)]
      [(cons id (op-delete _)) (delete id)]
      [else (raise (format "Illegal op: ~a" res-op))]
      ))

  (map delete-replacements (reverse(plan-ordered-operations plan)))

  (map process-op (plan-ordered-operations plan))
  #t)

(define (has-immutable-ref-to-replaced-resource? new acc-ops)
  (define (match? k v)
    (and (iref? v) (op-replace? (hash-ref acc-ops (ref->id v)))))
  (match-resource-attr? new match?))

(define (has-immutable-ref-to-updated-attr? new acc-ops) (ref-updated-attr? iref? new acc-ops))
(define (has-ref-to-updated-attr? new acc-ops)(ref-updated-attr? ref? new acc-ops))

(define (ref-updated-attr? test-fn? new acc-ops)
  (define (match? k v)
    (and (test-fn? v)
         ; TODO - BUG need to drill into changed attributes
         (op-update? (hash-ref acc-ops (ref->id v)))))
  (match-resource-attr? new match?))

(define (diff-resources new-module acc-ops old-res new-res)
  (define old-cfg (resource-config old-res))
  (define new-cfg (resource-config new-res))
  ; TODO - memoize diffs (store in operation?), also
  ; TODO - make-diff pattern here (see command.rkt) and parameter order
  (define munged (deref-resource new-module new-res))
  ; (pretty-print munged)
  (define diff (make-diff (hash-merge (resource-config munged) old-cfg) old-cfg))
  ; (pretty-print diff)
  (cond
    [(has-immutable-diff? diff) (op-replace "immutable diff")]
    [(has-immutable-ref-to-replaced-resource? new-cfg acc-ops) (op-replace "immutable ref to replaced resource")]
    [(has-immutable-ref-to-updated-attr? new-cfg acc-ops) (op-replace "immutable ref to updated attribute")]
    [(has-ref-to-updated-attr? new-cfg acc-ops) (op-update "ref to updated attribute")]
    [(has-diff? diff) (op-update "updated attribute")]
    [else #f]))

(define (operation id old-state new-module acc-ops)
  (if (hash-has-key? old-state id)
      (let ((new-res (resource-ref new-module id))
            (old-res (hash-ref old-state id)))
        (if (eq? (resource-driver old-res) (resource-driver new-res))
            (diff-resources new-module acc-ops old-res new-res)
            (op-replace "driver changed")))
      (op-create "New resource!")))

(define (get-plan-for mod #:refresh refresh)
  (define resources (rmodule-resources mod))
  (define resource-keyset (list->set (resource-keys mod)))
  ; TODO - pass in state, also define hash(id->state) contract types
  (define state-keyset (list->set (state-keys)))
  (define to-create (set-subtract resource-keyset state-keyset))
  (define to-delete (set-subtract state-keyset resource-keyset))
  (define to-refresh (set-subtract resource-keyset to-delete to-create))

  (cond [refresh (refresh (set->list to-refresh)) ])

  ; TODO - pass in state, also define hash(id->state) contract types
  (define new-module (mk-rmodule (rmodule-drivers mod) (merge-state+resource (mk-id->state) resources)))
  (define ordered-rks (map mod-id->id (resources-dag-topo mod)))
  ; (pretty-print ordered-rks)
  (define operations
    (foldl
     (lambda (id acc-ops)
       ; TODO - pass in state, also define hash(id->state) contract types
       (hash-set acc-ops id (operation id (mk-id->state) new-module acc-ops))) (hash) ordered-rks))

  (define ordered-ops
    (append
     (map (lambda (rk) (cons rk (op-delete "Removed from definition")))
          (sort (set->list to-delete) > #:key state-ref-serial))
     (map (lambda (rk) (cons rk (hash-ref operations rk)))
          ordered-rks)))

  (plan resources new-module ordered-ops))

; TODO - factor merge into the state module
(define (merge-state+resource old-resources new-resources)
  (make-immutable-hash
   (map (lambda(rk) (cons rk (state-merge (hash-ref old-resources rk #f)  (hash-ref new-resources rk))))
        (hash-keys new-resources))))

(define (filter-for-diffs meta-keys state-meta-map new-state-meta-map)
  (filter
   (lambda(rk)(has-diff?
               (make-diff
                (hash-ref state-meta-map rk) (hash-ref new-state-meta-map rk))))
   (set->list meta-keys)))
