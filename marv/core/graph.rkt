#lang racket/base

(require graph)
(require racket/contract)
(require racket/list)
(require racket/set)
(require "resources.rkt")
(require "resource-def.rkt")

(provide resources-dag-topo resources-dag resource-edges)

(define root '$root)

(define/contract (resources-dag-topo modl)
  (rmodule/c . -> . (listof res-id/c))
  (tsort (resources-dag modl)))

; TODO - don't need a full module?
(define/contract (resources-dag modl)
  (rmodule/c . -> . graph?)
  (define edges (append-map (lambda (k) (resource-edges k (resource-ref modl k))) (resource-keys modl)))
  (define graph (validated-graph edges modl))
  (remove-vertex! graph root)
  graph)

(define (validated-graph edges modl)
  (define graph (directed-graph edges))
  (define res-keys (set-add (list->set (resource-keys modl)) root))
  ; TODO - not right place to check/raise this?
  (when (not (dag? graph)) (raise "References are circular"))
  (define fails
    (filter-map
     (lambda (edge) (if (set-member? res-keys (first edge)) #f edge))
     (get-edges graph)))
  (if (empty? fails) graph
      (raise (format "Reference failures: ~a" fails))))

(define (resource-edges id res)
  (append (list(list root id)) (map (lambda (r) (list (ref->id (cdr r)) id)) (resource-refs res))))