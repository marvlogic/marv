#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/crud)
(require marv/log)

(provide storage-type-map)

(define (storage-type-map type) (crud-for-type (type-map) type))

(define base-type-map
  '#hasheq(
    (storage.bucketAccessControls . storage.bucketAccessControls)
    (storage.bucket . storage.buckets)
    (storage.channel . storage.channels)
    (storage.defaultObjectAccessControls . storage.defaultObjectAccessControls)
    (storage.notification . storage.notifications)
    (storage.objectAccessControls . storage.objectAccessControls)
    (storage.object . storage.objects)
    (storage.project . storage.projects)))

(define type-map (make-parameter base-type-map))