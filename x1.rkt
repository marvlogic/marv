#lang racket/base

(define state
#hash((resources
        .
        #hash((main.network.fw-health-check
               .
               #s(state-entry
                  10
                  #s(state-origin
                     #hasheq((driver . "gcp")
                             (name
                              .
                              "example-01-networking-fw-allow-health-check"))
                     #hasheq((api
                              .
                              #hasheq((api-id . "compute")
                                      (method . "DELETE")
                                      (required . ("(firewall project)"))
                                      (response-type
                                       .
                                       "compute.schemas.Operation")
                                      (url
                                       .
                                       "https://compute.googleapis.com/compute/beta/projects/{project}/global/firewalls/{name}")))
                             (config
                              .
                              #hasheq((allowed
                                       .
                                       (#hasheq((IPProtocol . "tcp"))))
                                      (creationTimestamp
                                       .
                                       "2024-04-19T07:28:51.432-07:00")
                                      (description . "")
                                      (direction . "INGRESS")
                                      (disabled . #f)
                                      (enableLogging . #f)
                                      (id . "316752909168525644")
                                      (kind . "compute#firewall")
                                      (logConfig . #hasheq((enable . #f)))
                                      (name
                                       .
                                       "example-01-networking-fw-allow-health-check")
                                      (network
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                                      (priority . 1000)
                                      (selfLink
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/firewalls/example-01-networking-fw-allow-health-check")
                                      (sourceRanges
                                       .
                                       ("130.211.0.0/22" "35.191.0.0/16"))
                                      (targetTags
                                       .
                                       ("load-balanced-backend"))))))
                  #hasheq((allowed . (#hasheq((IPProtocol . "tcp"))))
                          (creationTimestamp . "2024-04-19T07:28:51.432-07:00")
                          (description . "")
                          (direction . "INGRESS")
                          (disabled . #f)
                          (enableLogging . #f)
                          (id . "316752909168525644")
                          (kind . "compute#firewall")
                          (logConfig . #hasheq((enable . #f)))
                          (name
                           .
                           "example-01-networking-fw-allow-health-check")
                          (network
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                          (priority . 1000)
                          (project . "happyrat-liberauth")
                          (region . "europe-west1")
                          (selfLink
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/firewalls/example-01-networking-fw-allow-health-check")
                          (sourceRanges . ("130.211.0.0/22" "35.191.0.0/16"))
                          (targetTags . ("load-balanced-backend")))))
              (main.network.fw-proxies
               .
               #s(state-entry
                  13
                  #s(state-origin
                     #hasheq((driver . "gcp")
                             (name . "example-01-networking-fw-allow-proxies"))
                     #hasheq((api
                              .
                              #hasheq((api-id . "compute")
                                      (method . "DELETE")
                                      (required . ("(firewall project)"))
                                      (response-type
                                       .
                                       "compute.schemas.Operation")
                                      (url
                                       .
                                       "https://compute.googleapis.com/compute/beta/projects/{project}/global/firewalls/{name}")))
                             (config
                              .
                              #hasheq((allowed
                                       .
                                       (#hasheq((IPProtocol . "tcp")
                                                (ports . ("80")))
                                        #hasheq((IPProtocol . "tcp")
                                                (ports . ("443")))
                                        #hasheq((IPProtocol . "tcp")
                                                (ports . ("8080")))))
                                      (creationTimestamp
                                       .
                                       "2024-04-19T07:29:28.565-07:00")
                                      (description . "")
                                      (direction . "INGRESS")
                                      (disabled . #f)
                                      (enableLogging . #f)
                                      (id . "8959972471760470311")
                                      (kind . "compute#firewall")
                                      (logConfig . #hasheq((enable . #f)))
                                      (name
                                       .
                                       "example-01-networking-fw-allow-proxies")
                                      (network
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                                      (priority . 1000)
                                      (selfLink
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/firewalls/example-01-networking-fw-allow-proxies")
                                      (sourceRanges . ("10.0.2.0/24"))
                                      (targetTags
                                       .
                                       ("load-balanced-backend"))))))
                  #hasheq((allowed
                           .
                           (#hasheq((IPProtocol . "tcp") (ports . ("80")))
                            #hasheq((IPProtocol . "tcp") (ports . ("443")))
                            #hasheq((IPProtocol . "tcp") (ports . ("8080")))))
                          (creationTimestamp . "2024-04-19T07:29:28.565-07:00")
                          (description . "")
                          (direction . "INGRESS")
                          (disabled . #f)
                          (enableLogging . #f)
                          (id . "8959972471760470311")
                          (kind . "compute#firewall")
                          (logConfig . #hasheq((enable . #f)))
                          (name . "example-01-networking-fw-allow-proxies")
                          (network
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                          (priority . 1000)
                          (project . "happyrat-liberauth")
                          (region . "europe-west1")
                          (selfLink
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/firewalls/example-01-networking-fw-allow-proxies")
                          (sourceRanges . ("10.0.2.0/24"))
                          (targetTags . ("load-balanced-backend")))))
              (main.network.primary-sn
               .
               #s(state-entry
                  11
                  #s(state-origin
                     #hasheq((driver . "gcp")
                             (name . "example-01-networking-primary"))
                     #hasheq((api
                              .
                              #hasheq((api-id . "compute")
                                      (method . "DELETE")
                                      (required
                                       .
                                       ("(subnetwork region project)"))
                                      (response-type
                                       .
                                       "compute.schemas.Operation")
                                      (url
                                       .
                                       "https://compute.googleapis.com/compute/beta/projects/{project}/regions/{region}/subnetworks/{name}")))
                             (config
                              .
                              #hasheq((allowSubnetCidrRoutesOverlap . #f)
                                      (creationTimestamp
                                       .
                                       "2024-04-19T07:29:01.179-07:00")
                                      (description
                                       .
                                       "Primary subnet for example-01-networking")
                                      (fingerprint . "XdXnWKKJps8=")
                                      (gatewayAddress . "10.0.1.1")
                                      (id . "2389925277826269506")
                                      (ipCidrRange . "10.0.1.0/24")
                                      (kind . "compute#subnetwork")
                                      (name . "example-01-networking-primary")
                                      (network
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                                      (privateIpGoogleAccess . #f)
                                      (privateIpv6GoogleAccess
                                       .
                                       "DISABLE_GOOGLE_ACCESS")
                                      (purpose . "PRIVATE")
                                      (region . "europe-west1")
                                      (selfLink
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/regions/europe-west1/subnetworks/example-01-networking-primary")
                                      (stackType . "IPV4_ONLY")))))
                  #hasheq((allowSubnetCidrRoutesOverlap . #f)
                          (creationTimestamp . "2024-04-19T07:29:01.179-07:00")
                          (description
                           .
                           "Primary subnet for example-01-networking")
                          (fingerprint . "XdXnWKKJps8=")
                          (gatewayAddress . "10.0.1.1")
                          (id . "2389925277826269506")
                          (ipCidrRange . "10.0.1.0/24")
                          (kind . "compute#subnetwork")
                          (name . "example-01-networking-primary")
                          (network
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                          (privateIpGoogleAccess . #f)
                          (privateIpv6GoogleAccess . "DISABLE_GOOGLE_ACCESS")
                          (project . "happyrat-liberauth")
                          (purpose . "PRIVATE")
                          (region . "europe-west1")
                          (selfLink
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/regions/europe-west1/subnetworks/example-01-networking-primary")
                          (stackType . "IPV4_ONLY"))))
              (main.network.proxy-sn
               .
               #s(state-entry
                  12
                  #s(state-origin
                     #hasheq((driver . "gcp")
                             (name . "example-01-networking-proxy"))
                     #hasheq((api
                              .
                              #hasheq((api-id . "compute")
                                      (method . "DELETE")
                                      (required
                                       .
                                       ("(subnetwork region project)"))
                                      (response-type
                                       .
                                       "compute.schemas.Operation")
                                      (url
                                       .
                                       "https://compute.googleapis.com/compute/beta/projects/{project}/regions/{region}/subnetworks/{name}")))
                             (config
                              .
                              #hasheq((allowSubnetCidrRoutesOverlap . #f)
                                      (creationTimestamp
                                       .
                                       "2024-04-19T07:29:15.165-07:00")
                                      (description
                                       .
                                       "Proxy subnet for example-01-networking")
                                      (fingerprint . "4PW4bk_OXcI=")
                                      (gatewayAddress . "10.0.2.1")
                                      (id . "2554619651377054036")
                                      (ipCidrRange . "10.0.2.0/24")
                                      (kind . "compute#subnetwork")
                                      (name . "example-01-networking-proxy")
                                      (network
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                                      (privateIpGoogleAccess . #f)
                                      (privateIpv6GoogleAccess
                                       .
                                       "DISABLE_GOOGLE_ACCESS")
                                      (purpose . "REGIONAL_MANAGED_PROXY")
                                      (region . "europe-west1")
                                      (role . "ACTIVE")
                                      (selfLink
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/regions/europe-west1/subnetworks/example-01-networking-proxy")
                                      (state . "READY")))))
                  #hasheq((allowSubnetCidrRoutesOverlap . #f)
                          (creationTimestamp . "2024-04-19T07:29:15.165-07:00")
                          (description
                           .
                           "Proxy subnet for example-01-networking")
                          (fingerprint . "4PW4bk_OXcI=")
                          (gatewayAddress . "10.0.2.1")
                          (id . "2554619651377054036")
                          (ipCidrRange . "10.0.2.0/24")
                          (kind . "compute#subnetwork")
                          (name . "example-01-networking-proxy")
                          (network
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                          (privateIpGoogleAccess . #f)
                          (privateIpv6GoogleAccess . "DISABLE_GOOGLE_ACCESS")
                          (project . "happyrat-liberauth")
                          (purpose . "REGIONAL_MANAGED_PROXY")
                          (region . "europe-west1")
                          (role . "ACTIVE")
                          (selfLink
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/regions/europe-west1/subnetworks/example-01-networking-proxy")
                          (state . "READY"))))
              (main.network.vpc
               .
               #s(state-entry
                  9
                  #s(state-origin
                     #hasheq((driver . "gcp") (name . "example-01-networking"))
                     #hasheq((api
                              .
                              #hasheq((api-id . "compute")
                                      (method . "DELETE")
                                      (required . ("(project network)"))
                                      (response-type
                                       .
                                       "compute.schemas.Operation")
                                      (url
                                       .
                                       "https://compute.googleapis.com/compute/beta/projects/{project}/global/networks/{name}")))
                             (config
                              .
                              #hasheq((autoCreateSubnetworks . #f)
                                      (creationTimestamp
                                       .
                                       "2024-04-19T07:28:36.842-07:00")
                                      (description
                                       .
                                       "VPC for example-01-networking")
                                      (id . "8313409473705132411")
                                      (kind . "compute#network")
                                      (name . "example-01-networking")
                                      (networkFirewallPolicyEnforcementOrder
                                       .
                                       "AFTER_CLASSIC_FIREWALL")
                                      (routingConfig
                                       .
                                       #hasheq((routingMode . "REGIONAL")))
                                      (selfLink
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                                      (selfLinkWithId
                                       .
                                       "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/8313409473705132411")))))
                  #hasheq((autoCreateSubnetworks . #f)
                          (creationTimestamp . "2024-04-19T07:28:36.842-07:00")
                          (description . "VPC for example-01-networking")
                          (id . "8313409473705132411")
                          (kind . "compute#network")
                          (name . "example-01-networking")
                          (networkFirewallPolicyEnforcementOrder
                           .
                           "AFTER_CLASSIC_FIREWALL")
                          (project . "happyrat-liberauth")
                          (region . "europe-west1")
                          (routingConfig . #hasheq((routingMode . "REGIONAL")))
                          (selfLink
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/example-01-networking")
                          (selfLinkWithId
                           .
                           "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/global/networks/8313409473705132411")
                          (subnetworks
                           .
                           ("https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/regions/europe-west1/subnetworks/example-01-networking-proxy"
                            "https://www.googleapis.com/compute/beta/projects/happyrat-liberauth/regions/europe-west1/subnetworks/example-01-networking-primary")))))))
       (serial . 13)))