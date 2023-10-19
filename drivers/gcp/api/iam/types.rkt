#lang racket/base

(require racket/contract)
(require marv/drivers/gcp/crud)
(require marv/log)

(provide iam-type-map)

; Returns id of resource operation e.g. compute.instance.get
(define (iam-type-map type) (crud-for-type (type-map) type))

(define base-type-map
  #hasheq(
   (iam.organization.role . iam.organizations.roles)
   (iam.project.role . iam.projects.roles)
   (iam.project.serviceAccount . iam.projects.serviceAccounts)
   (iam.project.serviceAccount.key . iam.projects.serviceAccounts.keys)
   (iam.role . iam.role)))


(define type-map (make-parameter base-type-map))