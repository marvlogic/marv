#lang racket/base

(require "hash-utils.rkt")
(require "resource-def.rkt")
(require racket/pretty)

(provide make-diff
         has-diff?
         has-immutable-diff?
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

  (define (cmp k lhv)
    (define rh (hash-ref flat-rhs k void))
    (define rhv (update-val lhv (lambda (_) rh)))
    (values k
            (cond
              [(void? lhv) (removed rh)] ;; TODO - doesn't trigger
              [(void? rh) (added lhv)]
              [(toats-eq? lhv rhv) lhv]
              [else (changed lhv rhv)])))

  ; TODO - when used to diff spec -> state, there may be keys in state not in
  ; spec - don't know for sure if this is OK or if we need to merge keys from
  ; both hashes

  (define flat-lhs (make-immutable-hasheq (hash->flatlist lhs)))
  (define flat-rhs (make-immutable-hasheq (hash->flatlist rhs)))
  (hash-map/copy flat-lhs cmp))

(define (flatten-diff df)
  (define (flat kv acc)
    (define val (unpack-value (cdr kv)))
    (if (hash? val) (fltn val acc)
        (cons (cons (car kv) val) acc)))

  (define (fltn d init) (foldl flat init (hash->list d)))

  (fltn df '()))

(define (has-diff? df) (hash-match? df (lambda (_ v) (diff? v))))

; TODO - collect with other has-*diff? functions
(define (has-immutable-diff? df)
  (define (chkdiff k v)
    (or (and (changed? v) (ival? (changed-new v)))
        (and (removed? v) (ival? (removed-old v))) ))
  (hash-match? df chkdiff))
