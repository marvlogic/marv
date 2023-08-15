#lang racket/base

(require marv/utils/hash)
(require marv/core/resource-def)
(require marv/drivers/gcp/api)
(require marv/drivers/dev)

(provide drivers resources)

; This resource specification is the marv equivalent of this:
;    https://cloud.google.com/load-balancing/docs/https/setting-up-reg-ext-https-lb#gcloud_6
;
; It works but it's not meant to be a good example, as it needs refactoring!

(define (getenv-or-raise e)
  (or (getenv e) (raise (format "ERROR: ~a must be defined in environment" e))))

(define (drivers #:node-size (node-size "f1-micro"))
  (hash
   'dev (init-dev-driver 'dev)
   'gcp (init-gcp 'gcp (gcp-http-transport (getenv-or-raise "GCP_ACCESS_TOKEN"))
                  #:project (hash-ref defaults 'project)
                  #:region (hash-ref defaults 'region)
                  )))

(define defaults
  (hash 'project (getenv-or-raise "MARV_GCP_PROJECT")
        'region  (getenv-or-raise "MARV_GCP_REGION")))

(define vpc
  (make-immutable-hasheq
   `(
     (name . ,(ival "vpc1"))
     (description . "vpc1")
     (autoCreateSubnetworks . #f)
     (routingConfig . #hasheq((routingMode . "REGIONAL")))
     )))

(define sn1
  (make-immutable-hasheq
   `(
     (name . "subnet1")
     (description . "primary subnet")
     (ipCidrRange . ,(ival "10.0.1.0/24"))
     (region . "europe-west2")
     (network . ,(iref 'vpc.selfLink))
     ; TODO - fingerprint issue
     ;  (tags . #hasheq((x . "y")))
     )))

(define proxy-sn
  (make-immutable-hasheq
   `(
     (name . "proxy-only-subnet")
     (description . "subnet for proxy")
     (ipCidrRange . ,(ival "10.0.3.0/24"))
     (region . "europe-west2")
     (network . ,(iref 'vpc.selfLink))
     (purpose . "REGIONAL_MANAGED_PROXY")
     (role . "ACTIVE")
     )))

(define fw-health-check
  (make-immutable-hasheq
   `(
     (name . "fw-allow-health-check")
     (network . ,(iref 'vpc.selfLink))
     (sourceRanges . ("130.211.0.0/22" "35.191.0.0/16"))
     (targetTags .  ("load-balanced-backend"))
     (allowed . (#hasheq((IPProtocol . "tcp"))))
     (direction . "INGRESS")
     )))

(define fw-proxies
  (make-immutable-hasheq
   `(
     (name . "fw-allow-proxies")
     (network . ,(iref 'vpc.selfLink))
     (sourceRanges . (,(ref 'proxy-sn.ipCidrRange)))
     (targetTags .  ("load-balanced-backend"))
     (allowed . (#hasheq( (IPProtocol . "tcp") (ports . ("80")))
                 #hasheq( (IPProtocol . "tcp") (ports . ("443")))
                 #hasheq( (IPProtocol . "tcp") (ports . ("8080")))
                 ))
     (direction . "INGRESS")
     )))

(define node-disks
  (list
   #hasheq(
    (boot . #t)
    (autoDelete . #t)
    (initializeParams
     .  #hasheq(
         (sourceImage . "projects/debian-cloud/global/images/family/debian-12")
         (diskSizeGb . "10")
         ;  (diskType . "zones/europe-west2-a/diskTypes/pd-standard")
         )))))

(define (instance-template node-size)
  (hasheq
   'name  "l7-xlb-backend-template"
   'properties (hash
                'machineType node-size
                'tags #hasheq((items . ("load-balanced-backend")))
                'networkInterfaces (list
                                    (hasheq
                                     'network (iref 'vpc.selfLink)
                                     'subnetwork (iref 'sn1.selfLink)
                                     'accessConfigs (list #hasheq((type . "ONE_TO_ONE_NAT")))))
                'disks node-disks)))

(define (instance-group-manager name size zone template)
  (make-immutable-hasheq
   `(
     (name . ,name)
     (zone . ,zone)
     (namedPorts . ,(ival (list #hasheq((port . 40) (name . "http")))))
     (instanceTemplate . ,(iref template))
     (baseInstanceName . ,name)
     (targetSize . ,size)
     )))



(define lb-external-ip (hash 'name "LB External IP" 'networkTier "STANDARD"))

(define lb-basic-check
  #hasheq(
   (name . "lb-basic-check")
   (type . "HTTP")
   (httpHealthCheck . #hasheq(
                       (portSpecification . "USE_SERVING_PORT")))))

(define region-backend-service
  (make-immutable-hasheq
   `((name . "backend-service")
     (backends . ,(hash 'group (iref 'instance-group-manager-a.selfLink)
                        'balancingMode "UTILIZATION"))
     (healthChecks . (,(ref 'lb-basic-check.selfLink)))
     (loadBalancingScheme . "EXTERNAL_MANAGED"))))

(define region-url-map
  (make-immutable-hasheq
   `((name . "urlmap")
     (defaultService . ,(iref 'region-backend-service.selfLink)))))

(define region-target-proxies
  (make-immutable-hasheq
   `((name . "proxy")
     (urlMap . ,(iref 'region-url-map.selfLink)))))

(define forwarding-rule
  (make-immutable-hasheq
   `((name . "forwarding-rule")
     (ipAddress . "10.0.1.99") ; backend SN1
     (ipProtocol . "TCP")
     (portRange . "80-80")
     (target . ,(iref 'region-target-proxies.selfLink))
     (loadBalancingScheme . "EXTERNAL_MANAGED")
     (network . ,(iref 'vpc.selfLink))
     (networkTier . "STANDARD"))))

(define (no-resources mkres #:node-size (node-size "f1-micro")) (list))

(define (stack mkres #:node-size (node-size "f1-micro"))

  (define (resf type res) (hash-merge res (hash-set defaults '$type (ival type))))
  (define (gcp t r) (mkres 'gcp (resf t r)))

  `((vpc . ,(gcp "compute.network" vpc))
    (sn1 .  ,(gcp "compute.subnetwork" sn1))
    (proxy-sn .  ,(gcp "compute.subnetwork" proxy-sn))
    (fw-health . ,(gcp "compute.firewall" fw-health-check))
    (fw-proxies . ,(gcp "compute.firewall" fw-proxies))
    (instance-template . ,(gcp "compute.instanceTemplate" (instance-template node-size)))
    (instance-group-manager-a . ,(gcp "compute.instanceGroupManager"
                                      (instance-group-manager "example" 2 "europe-west2-a" 'instance-template.selfLink)))
    ;    (instance-group-manager-c .
    ;          ,(gcp "compute.instanceGroupManager"
    ;               (instance-group-manager "example" 2 "europe-west2-c" 'instance-template.selfLink)))

    ;    (lb-external-ip (gcp "compute.address" lb-external-ip))
    (lb-basic-check . ,(gcp "compute.regionHealthCheck" lb-basic-check))
    (region-backend-service . ,(gcp "compute.regionBackendService" region-backend-service))
    (region-url-map . ,(gcp "compute.regionUrlMap" region-url-map))
    (region-target-proxies . ,(gcp "compute.regionTargetHttpProxy" region-target-proxies))
    (forwarding-rule . ,(gcp "compute.forwardingRule" forwarding-rule))
    ))

(define resources (if (getenv "PURGE") no-resources stack))