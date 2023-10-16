#lang racket/base

(require racket/set)
(require racket/list)
(require marv/utils/hash)
(require marv/core/resource-def)

(provide make-diff
         has-diff?
         has-immutable-diff?
         diff?
         (struct-out changed)
         (struct-out added)
         (struct-out removed))

(struct diff () #:prefab)
(struct changed diff (new old) #:prefab)
(struct added diff (new) #:prefab)
(struct removed diff (old) #:prefab)

(define (make-diff lhs rhs)
  ; Changes in lhs (new) being applied to rhs (old)

  ; TODO - think about functions to get left and right side of results from reading a diff?

  (define (toats-eq? lh rh) (or (eq? lh rh) (equal? lh rh)))

  (define flat-lhs (make-immutable-hasheq (hash->flatlist lhs)))
  (define flat-rhs (make-immutable-hasheq (hash->flatlist rhs)))
  (define all-keys (list->set (flatten (list (hash-keys flat-lhs) (hash-keys flat-rhs)))))

  (define (cmp k)
    (define lhv (hash-ref flat-lhs k void))
    (define rh (hash-ref flat-rhs k void))
    (define rhv (update-val lhv (lambda (_) rh)))
    (values k
            (cond
              [(void? lhv) (removed rh)]
              [(void? rh) (added lhv)]
              [(toats-eq? lhv rhv) lhv]
              [else (changed lhv rhv)])))

  (for/hash ([k all-keys]) (cmp k)))

(define (has-diff? df) (hash-match? df (lambda (_ v) (diff? v))))

; TODO - collect with other has-*diff? functions
(define (has-immutable-diff? df)
  (define (chkdiff k v)
    (or (and (changed? v) (ival? (changed-new v)))
        (and (removed? v) (ival? (removed-old v))) ))
  (hash-match? df chkdiff))
