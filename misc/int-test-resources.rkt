#lang racket/base


(require marv/utils/hash)
(require marv/core/resource-def)
(require marv/drivers/gcp/api)
(require marv/drivers/dev)

(provide drivers resources)

(define (drivers #:skip-define (skip #f) #:storage-class (storage-class "STANDARD"))
  (hash
   'gcp (init-dev-driver 'gcp)
   ;  'gcp2 (init-gcp 'gcp (gcp-http-transport (getenv "GCP_ACCESS_TOKEN"))
   ;                  #:project "marv-224713"
   ;                  #:region "EUROPE-WEST2")
   ))

(define ACCESS-TOKEN (getenv "GCP_ACCESS_TOKEN"))
(define gcp-driver
  #hasheq(
   (project . "marv")
   (region . "EUROPE-WEST2")
   (access-token . ACCESS-TOKEN)))

(define rak1
  #hasheq(
   (iamConfiguration
    .
    #hasheq((bucketPolicyOnly .  #hasheq((enabled . #t)))
            (publicAccessPrevention . "inherited")
            (uniformBucketLevelAccess .  #hasheq((enabled . #t)))))
   (location . "EUROPE-WEST2")
   (name . "marv-rak1")
   (storageClass . "STANDARD")))

(define rak2
  #hasheq(
   (iamConfiguration
    .
    #hasheq((bucketPolicyOnly .  #hasheq((enabled . #t)))
            (publicAccessPrevention . "inherited")
            (uniformBucketLevelAccess .  #hasheq((enabled . #t)))))
   (location . "EUROPE-WEST2")
   (name . "marv-rak2")
   (storageClass . "STANDARD")))

(define rak3
  #hasheq(
   (location . "EUROPE-WEST2")
   (name . "marv-rak3")
   (iamConfiguration
    .
    #hasheq((bucketPolicyOnly .  #hasheq((enabled . #t)))
            (publicAccessPrevention . "inherited")
            ; NB uniformBucketLevelAccess required to prevent ACL by default
            ; TODO -  use case for policy filtering
            (uniformBucketLevelAccess .  #hasheq((enabled . #t)))))
   (storageClass . "NEARLINE")))


(define vpc1
  (make-immutable-hash
   `(
     (name . ,(ival "vpc-rak1"))
     (description . "VPC for RAK")
     (autoCreateSubnetworks . #f)
     (routingConfig . #hasheq((routingMode . "REGIONAL")))
     )))

(define sn1
  (make-immutable-hasheq
   `(
     (name . "subnet-rak1")
     (description . "RAK2 subnet")
     (ipCidrRange . ,(ival "10.0.1.0/24"))
     (region . "europe-west2")
     (network . ,(iref 'vpc1.selfLink))
     )))

(define ci1
  (make-immutable-hash
   `(
     (name . "instance1")
     (description . "CI instance")
     (type . ,(ival "SMALL"))
     (subnetId . ,(iref 'sn1.selfLink))
     )))

(define defaults
  #hasheq( (project . "marv-224713" ) ))

(define (resources mkres #:skip-define (skip #f) #:storage-class (storage-class "STANDARD"))
  (define (resf type res) (hash-merge res (hash-set defaults '$type (ival type))))
  (if skip (list)
      (list
       (cons 'rak1  (mkres 'gcp (resf "storage.bucket" (hash-set rak1 'storageClass storage-class))))
       (cons 'vpc1 (mkres 'gcp (resf "compute.network" vpc1)))
       (cons 'sn1  (mkres 'gcp (resf "compute.subnetwork" sn1)))
       (cons 'sn2  (mkres 'gcp (resf "compute.subnetwork"
                                     (hash-set* sn1 'name "subnet-rak2" 'ipCidrRange (ival "10.0.2.0/24")))))
       ;  (cons 'ci1  (mkres 'gcp  (resf "compute.instance" ci1)))
       ;  (cons 'ci2  (mkres 'gcp  (resf "compute.instance" (hash-set ci1 'name "instance2"))))
       )))
