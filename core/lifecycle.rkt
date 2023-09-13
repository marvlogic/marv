#lang racket/base

(require racket/set)
(require racket/dict)
(require racket/function)
(require racket/match)
(require racket/pretty)
(require racket/hash)
(require racket/string)

(require marv/log)
(require marv/core/state)
(require marv/core/diff)
(require marv/core/resource-def)
(require marv/core/resources)
(require marv/core/graph)
(require marv/utils/hash)
(require marv/drivers/driver)

(provide import-resources
         plan-changes
         apply-changes
         output-plan
         (struct-out plan))

(struct plan (resources desired-state ordered-operations) #:prefab)

(struct op-base (reason) #:transparent)
(struct op-create(op-base) #:transparent)
(struct op-replace(op-base diff) #:transparent)
(struct op-update(op-base diff) #:transparent)
(struct op-delete(op-base) #:transparent)

(define (plan-changes mod (refresh? #t))
  (get-plan-for mod refresh?))

(define (get-plan-for mod refresh?)
  (define resources (rmodule-resources mod))
  (define resource-keyset (list->set (resource-keys mod)))
  ; TODO - pass in state, also define hash(id->state) contract types
  (define state-keyset (list->set (state-keys)))
  (define to-create (set-subtract resource-keyset state-keyset))
  (define to-delete (set-subtract state-keyset resource-keyset))
  (define to-refresh (set-subtract resource-keyset to-delete to-create))

  ; (cond [refresh (refresh (set->list to-refresh)) ])

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

(define (import-resources mod ids)
  (define (import-one id)
    (displayln (format "importing ~a" id)))
  ; (state-set-ref id
  ;                ((driver-read md)
  ;                 (unwrap-values(deref-resource
  ;                                (rmodule (rmodule-drivers mod) (mk-id->state)) (resource-ref mod id))))))

  (map (lambda(id) (import-one id)) ids))

(define (mk-refresh-func readfn)
  (define (update k)
    (displayln (format "Refreshing ~a" k))
    (state-set-ref k (readfn (state-ref k))))
  (lambda (resources-meta) (map update resources-meta)))

(define (module-crudfn m) (driver-crudfn (make-driver-for-set (rmodule-drivers m))))

(define (apply-changes mod (refresh? #t))
  (define crudfn (module-crudfn mod))

  (define (create id)
    ; (pretty-print (driver-repr id))
    (display (format "CREATING ~a" id))
    (flush-output)
    (state-set-ref id (crudfn 'create (driver-repr id))))

  (define (delete id)
    (display (format "DELETING ~a" id))
    (flush-output)
    (crudfn 'delete (state-ref id))
    (state-delete id))

  (define (update id)
    (display (format "UPDATING ~a" id))
    (flush-output)
    (state-set-ref id (crudfn 'update (driver-repr id))))

  ; TODO - check if unpacking is done here or should use unwrap fn
  (define (driver-repr id)
    (unwrap-values
     (deref-resource (rmodule (rmodule-drivers mod) (mk-id->state))
                     (resource-ref mod id))))

  (define plan (get-plan-for mod refresh?))

  (define (delete-replacements res-op)
    (match res-op
      [(cons id (op-replace _ _)) (delete id)]
      [_ void]))

  (define (process-op res-op)
    (match res-op
      [(cons id #f) void]
      [(cons id (op-create _)) (create id)]
      [(cons id (op-replace _ _)) (create id)]
      [(cons id (op-update _ _)) (update id)]
      [(cons id (op-delete _)) (delete id)]
      [else (raise (format "Illegal op: ~a" res-op))]
      ))

  (for-each delete-replacements (reverse(plan-ordered-operations plan)))
  (for-each process-op (plan-ordered-operations plan)))

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
    [(has-immutable-diff? diff) (op-replace "immutable diff" diff)]
    [(has-immutable-ref-to-replaced-resource? new-cfg acc-ops) (op-replace "immutable ref to replaced resource" diff)]
    [(has-immutable-ref-to-updated-attr? new-cfg acc-ops) (op-replace "immutable ref to updated attribute" diff)]
    [(has-ref-to-updated-attr? new-cfg acc-ops) (op-update "ref to updated attribute" diff)]
    [(has-diff? diff) (op-update "updated attribute" diff)]
    [else #f]))

(define (resource-driver x)#t)
(define (operation id old-state new-module acc-ops)
  ; (log-marv-debug "Checking ~a vs old ~a" id old-state)
  (if (hash-has-key? old-state id)
      (let ((new-res (resource-ref new-module id))
            (old-res (hash-ref old-state id)))
        (if (eq? (resource-driver old-res) (resource-driver new-res))
            (diff-resources new-module acc-ops old-res new-res)
            (op-replace "driver changed")))
      (op-create "New resource!")))

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

(define (output-plan plan)

  (define (report-op rid op)

    (define (fmt-diff d)
      (match d
        [(changed new old) (format "~a -> ~a" (unpack-value old) (unpack-value new))]
        [(added new) (format "~a (added)" (unpack-value new))]
        [(removed old) (format "~a (removed)" (unpack-value old))]))

    (define (report-diff d)
      (define diffs (filter (lambda (p) (diff? (cdr p))) (hash->flatlist d)))
      (dict-map diffs
                (lambda (k v)
                  (displayln (format "  ~a: ~a" k (fmt-diff v))))))

    (define (header id msg) (displayln (format "~a -> ~a" id msg)))

    (match op
      [(op-create reason) (header rid reason )]
      [(op-delete reason) (header rid reason)]
      [(op-update reason diff) (header rid reason)
                               (report-diff  diff)]
      [(op-replace reason diff) (header rid (format "REPLACE: ~a" reason))
                                (report-diff diff)]
      [else void]
      ))

  (for-each
   (lambda (op) (report-op (car op) (cdr op)))
   (plan-ordered-operations plan)))
