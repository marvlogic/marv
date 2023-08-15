#lang racket/base

(require marv/utils/hash)
(require marv/core/resource-def)
(require marv/drivers/gcp/api)
(require marv/drivers/dev)

(provide drivers resources)

(define (getenv-or-raise e)
  (or (getenv e) (raise (format "ERROR: ~a must be defined in environment" e))))

(define (drivers #:node-size (node-size "SMALL"))
  (hash
   'gcp2 (init-dev-driver 'gcp)
   'gcp (init-gcp 'gcp (gcp-http-transport (getenv "GCP_ACCESS_TOKEN"))
                  #:project (hash-ref defaults 'project)
                  #:region (hash-ref defaults 'region)
                  )))

(define defaults
  (hash 'project (getenv-or-raise "MARV_GCP_PROJECT")
        'region  (getenv-or-raise "MARV_GCP_REGION")))

(define vpc
  (make-immutable-hash
   `(
     (name . ,(ival "vpc"))
     (description . "vpc")
     (autoCreateSubnetworks . #f)
     (routingConfig . #hasheq((routingMode . "REGIONAL")))
     )))

(define sn1
  (make-immutable-hasheq
   `(
     (name . "subnet")
     (description . "subnet")
     (ipCidrRange . ,(ival "10.0.1.0/24"))
     (region . "europe-west2")
     (network . ,(iref 'vpc.selfLink))
     )))

(define node-disks
  (list
   #hasheq(
    (boot . #t)
    (initializeParams
     .  #hasheq(
         (sourceImage . "projects/debian-cloud/global/images/family/debian-12")
         (diskSizeGb . "10")
         (diskType . "zones/europe-west2-a/diskTypes/pd-standard"))))))

(define (node-nw snref)
  (list
   (make-immutable-hasheq
    (list (cons 'subnetwork (iref snref))))))

(define (node-def name desc mach-type zone subnet-ref)
  (define (machine-type t)(format "zones/~a/machineTypes/~a" zone t))
  (make-immutable-hasheq
   `(
     (name . ,name)
     (description . ,desc)
     (machineType . ,(ival (machine-type mach-type)))
     (networkInterfaces . ,(node-nw subnet-ref))
     (disks . ,node-disks)
     (zone . ,zone)
     )))

(define (nowt mkres #:node-size (node-size "f1-micro")) (list))
(define (stack mkres #:node-size (node-size "f1-micro"))

  (define (resf type res) (hash-merge res (hash-set defaults '$type (ival type))))
  (list
   (cons 'vpc (mkres 'gcp (resf "compute.network" vpc)))
   (cons 'sn1  (mkres 'gcp (resf "compute.subnetwork" sn1)))
   (cons 'node1
         (mkres 'gcp
                (resf "compute.instance"
                      (node-def "node1" "cluster node 1" node-size "europe-west2-a"'sn1.selfLink))))
   ))

(define resources (if (getenv "PURGE") nowt stack))