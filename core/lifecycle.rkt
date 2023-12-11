#lang racket/base

(require racket/set)
(require racket/dict)
(require racket/match)
(require racket/pretty)

(require marv/core/values)
(require marv/core/state)
(require marv/core/diff)
(require marv/core/resource-def)
(require marv/core/resources)
(require marv/core/graph)
(require marv/utils/hash)
(require marv/core/drivers)
(require marv/log)

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
  (define resource-keyset (list->set (resource-keys mod)))
  ; TODO - pass in state, also define hash(id->state) contract types
  (define state-keyset (list->set (state-keys)))
  (define to-create (set-subtract resource-keyset state-keyset))
  (define to-delete (set-subtract state-keyset resource-keyset))
  (define to-refresh (set-subtract resource-keyset to-delete to-create))

  (when refresh? (refresh-resources mod (set->list to-refresh)))

  ; TODO - pass in state, also define hash(id->state) contract types
  (define new-module (merge-state+resource (mk-id->state) mod))
  (define ordered-rks (resources-dag-topo mod))
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

  (plan mod new-module ordered-ops))

(define (import-resources mod ids)
  ; (define (import-one id)
  ;    (displayln (format "importing ~a" id))
  ;    (define res
  ;      (unwrap-values
  ;       (deref-resource
  ;        (rmodule (rmodule-drivers mod) (mk-id->state)) (resource-ref mod id))))
  ;    (define driver-id (resource-driver-id res))
  ;    (define config ((current-driver) driver-id 'read (resource-config res)))
  ;    (state-set-ref id (resource driver-id config)))

  ; (map (lambda(id) (null id)) ids))
  (raise "unimplemented"))

(define (refresh-resources mod ids)
  ; TODO41
  ; (define (readr res)
  ;   (define did (resource-driver-id res))
  ;   (resource did ((current-driver) did 'read (resource-config res))))

  ; (define (refresh k)
  ;   (displayln (format "Refreshing ~a" k))
  ;   (define res (state-ref k))
  ;   (state-set-ref k (readr res)))
  ; (for ([i ids]) (refresh i)))
  void)

(define (apply-changes mod (refresh? #t))

  (define (crudfn op res)
    (define type-fn (resource-type-fn res))
    (type-fn op (resource-config res)))

  (define (create id)
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
    (define res (hash-ref mod id))
    (resource (resource-type-fn res)
              (unwrap-values (deref-resource (mk-id->state) (resource-config res)))))

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
  (define (is-ref-a-diff? ref)
    (diff? (get-ref-diff ref acc-ops)))
  (define (match? k v)
    (and (test-fn? v) (is-ref-a-diff? v)))
  (match-resource-attr? new match?))

(define (get-ref-diff ref acc-ops)
  (define-values (id attr) (ref-split ref))
  (match (hash-ref acc-ops id)
    [(op-update _ diff) (hash-ref diff attr #f)]
    [(op-replace _ diff) (hash-ref diff attr #f)]
    [else #f]))


(define (diff-resources new-module acc-ops old-cfg new-cfg)
  ; (define old-cfg (resource-config old-res))
  ; (define new-cfg (resource-config new-res))
  ; TODO - memoize diffs (store in operation?), also
  ; TODO - make-diff pattern here (see command.rkt) and parameter order
  (define munged (deref-resource new-module new-cfg))
  (log-marv-debug "deref'd resource: ~a" munged)
  (define (flathash h) (make-immutable-hasheq (hash->flatlist h)))
  (define diff (make-diff (hash-merge (flathash munged)
                                      (flathash old-cfg)) (flathash old-cfg)))
  (cond
    [(has-immutable-diff? diff) (op-replace "immutable diff" diff)]
    [(has-immutable-ref-to-replaced-resource? new-cfg acc-ops) (op-replace "immutable ref to replaced resource" diff)]
    [(has-immutable-ref-to-updated-attr? new-cfg acc-ops)
     (op-replace "immutable ref to updated attribute" diff)]
    [(has-ref-to-updated-attr? new-cfg acc-ops) (op-update "ref to updated attribute" diff)]
    [(has-diff? diff) (op-update "updated attribute" diff)]
    [else #f]))

(define (operation id old-state new-module acc-ops)
  (log-marv-debug "Checking diffs for ~a" id)
  (if (hash-has-key? old-state id)
      (let ((new-res (hash-ref new-module id))
            (old-res (hash-ref old-state id)))
        (log-marv-debug " old-state: ~a" old-res)
        (log-marv-debug " new-state: ~a" new-res)
        (define diff (diff-resources new-module acc-ops old-res new-res))
        (log-marv-debug " diff: ~a" diff)
        diff)
      (op-create "New resource!")))

; TODO - factor merge into the state module
(define (merge-state+resource old-resources new-resources)
  (make-immutable-hash
   (map
    (lambda(rk)
      (cons rk (state-merge (hash-ref old-resources rk state-empty)  (hash-ref new-resources rk))))
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
