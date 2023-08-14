#lang racket/base

(provide storage-type-map)
(define storage-type-map
  '#hash((bucketAccessControls . bucketAccessControls)
         (bucket . buckets)
         (channel . channels)
         (defaultObjectAccessControls . defaultObjectAccessControls)
         (notification . notifications)
         (objectAccessControls . objectAccessControls)
         (object . objects)
         (project . projects))
  )