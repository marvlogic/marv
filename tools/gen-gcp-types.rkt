#lang racket/base

(provide (all-defined-out))

(define (gen-types base-type-map)
  (displayln #<<EOF
#lang marv
import "../common.mrv"

EOF
             )
  (for ([kv (in-list (hash->list base-type-map))])
    (define-values (typ at) (values (car kv) (cdr kv)))
    (displayln
     (format
      #<<EOF
type gcp:~a = {
  create = ~a.insert { identity identity }
  read = ~a.get { identity identity }
  update = ~a.patch { identity identity }
  delete = ~a.delete { identity identity }
}

EOF
      typ at at at at
      ))))

(define btm
  #hasheq(
   (sql.backupRun . sql.backupRuns)
   (sql.connect . sql.connect)
   (sql.database . sql.databases)
   (sql.flag . sql.flags)
   (sql.instance . sql.instances)
   (sql.operation . sql.operations)
   (sql.project.instance . sql.projects.instances)
   (sql.sslCert . sql.sslCerts)
   (sql.tier . sql.tiers)
   (sql.user . sql.users)
   ))
(with-output-to-file )