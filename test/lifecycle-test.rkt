#lang racket/base

'lifecycle-test

(require rackunit)

(require "lifecycle.rkt")
(require "state.rkt")

(require "resources.rkt")
(require "resource-def.rkt")
(require marv/core/diff)
(require/expose
 marv/core/lifecycle
 (op-replace
  op-update
  op-delete
  get-plan-for
  has-immutable-ref-to-replaced-resource?
  has-immutable-ref-to-updated-attr?
  has-ref-to-updated-attr?))

(define acc-ops
  (make-immutable-hash
   `(  (main.bucket1  . #f)
       (main.bucket2  . ,(op-replace "immutable" (hash)))
       (main.bucket3  . ,(op-update "blah" (hash 'attr1 (changed 3 2 )))))))

(define res1
  (make-immutable-hash
   `(
     (name . "blah")
     (region . ,(ival (ref 'main.bucket1/attr1)))
     )))

(check-equal? (has-immutable-ref-to-replaced-resource? res1 acc-ops) #f)

(define res2 (hash-set res1 'region (ival(ref 'main.bucket2/attr1))))
(define list-nested-iref (hash-set res1 'region (list (iref 'main.bucket2/attr1))))

(check-equal? (has-immutable-ref-to-replaced-resource? res1 acc-ops) #f)
(check-equal? (has-immutable-ref-to-replaced-resource? res2 acc-ops) #t)
(check-equal? (has-immutable-ref-to-replaced-resource? list-nested-iref acc-ops) #t)

(define res3 (hash-set res1 'region (ival(ref 'main.bucket3/attr1))))
(check-equal? (has-immutable-ref-to-updated-attr? res1 acc-ops) #f)
(check-equal? (has-immutable-ref-to-updated-attr? res3 acc-ops) #t)

(define res4 (hash-set res1 'region (ref 'main.bucket3/attr1)))
(check-equal? (has-ref-to-updated-attr? res1 acc-ops) #f)
(check-equal? (has-ref-to-updated-attr? res4 acc-ops) #t)

(define (setup-state resources)
  (map (lambda (res) (state-set-ref (car res) (resource 'gcp (cdr res)))) resources))

(around
 (setup-state (list (cons 'res1 res1) (cons 'res2 res2) (cons 'res3 res3)))
 (check-equal? (plan-ordered-operations
                (get-plan-for (mk-rmodule (hash) (hash)) #f))
               (list
                (cons 'res3 (op-delete "Removed from definition"))
                (cons 'res2 (op-delete "Removed from definition"))
                (cons 'res1 (op-delete "Removed from definition"))))
 (map state-delete '(res1 res2 res3))
 )