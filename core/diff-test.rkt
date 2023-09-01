#lang racket/base

(require rackunit
         racket/function
         racket/pretty
         marv/core/diff
         marv/core/resource-def
         marv/utils/hash)

'diff-test

(define current-state
  (make-immutable-hash
   `( (name . "marv-rak1")
      (pid . ,(identity "marv"))
      (onlyold . "only-in-old")
      (list . (1  2 3 4))
      (nested-immutable . ,(hash 'field1 "changeme" 'field2 "removeme"))
      (perms . #hash((readers . "kev")(writers . "bob")))
      (tags . #hash((val1 . "kev")(val2 . "bob")))
      (region . "europe-west2"))))

(define spec-for-state
  (make-immutable-hash
   `( (name . "marv-rak1")
      (pid . ,(identity "marv"))
      (onlyold . "only-in-old")
      (list . (1 2 3 4))
      (nested-immutable . ,(hash 'field1 (ival "changeme") 'field2 (ival "removeme")))
      (perms . #hash((readers . "kev")(writers . "bob")))
      (tags . ,(ival #hash((val1 . "kev")(val2 . "bob"))))
      (region . "europe-west2"))))

(define immutable-changed
  (make-immutable-hash
   `( (name . "marv-rak1")
      (pid . ,(identity "marv"))
      (onlyold . "only-in-old")
      (list . (list 1  2 3 4))
      (perms . #hash((readers . "kev")(writers . "bob")))
      (tags . ,(ival #hash((val1 . "changed")(val2 . "bob"))))
      (region . "europe-west2"))))

(define immutable-nested-changed
  (make-immutable-hash
   `( (name . "marv-rak1")
      (pid . ,(identity "marv"))
      (onlyold . "only-in-old")
      (nested-immutable . ,(hash 'field1 (ival "imchanged")))
      (list . (list 1  2 3 4))
      (perms . #hash((readers . "kev")(writers . "bob")))
      (tags . ,(ival #hash((val1 . "kev")(val2 . "bob"))))
      (region . "europe-west2"))))

(define updated1
  (make-immutable-hash
   `((name . "marv-rak1")
     (pid . ,(ival "marv"))
     (list . (list 1  2 3))
     (onlynew . "only-in-new")
     (perms .  #hash((executors . "kev") (readers .  "kev,bob")))
     (tags . ,(ival #hash((val1 . "kev")(val2 . "bob"))))
     (region . "europe-west3"))))

(define nested-list1
  #hasheq(
   (sourceRanges . (#s(value "10.0.2.0/24" (immutable))))
   (allowed
    .
    (#hasheq((IPProtocol . "tcp") (ports . ("80")))
     #hasheq((IPProtocol . "tcp") (ports . ("443")))
     #hasheq((IPProtocol . "tcp") (ports . ("8080")))))
   (kind . "compute#firewall")
   (logConfig . #hasheq((enable . #f)))
   (name . "fw-allow-proxies")
   (targetTags . ("load-balanced-backend"))))

(check-false (has-diff? (make-diff spec-for-state current-state)))
(check-true (has-diff? (make-diff updated1 current-state)))
(check-false (has-immutable-diff? (make-diff updated1 current-state)))

(check-false (has-immutable-diff? (make-diff spec-for-state current-state)))
(check-true (has-immutable-diff? (make-diff immutable-changed current-state)))
(check-true (has-immutable-diff? (make-diff immutable-nested-changed current-state)))

(define nl2 (hash-set nested-list1 'sourceRanges '("10.0.2.0/24")))
(define (newhash hs) ((compose1 make-immutable-hash hash->flatlist) hs))
(check-false (has-diff?  (make-diff nested-list1 nl2)))

; (pretty-print (make-diff (newhash nested-list1) (newhash nl2)))
