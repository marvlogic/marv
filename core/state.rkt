#lang racket/base
(require racket/hash)
(require racket/contract)
(require marv/core/resources)
(require marv/core/config)
(require marv/core/values)
(require marv/utils/hash)

(provide load-state
         save-state
         state-keys
         state-set-ref
         state-ref
         state-ref-config
         state-ref-origin
         state-ref-serial
         state-ref-destructor-cmd
         state-origin-destructor-cmd
         state-origin-fingerprint
         state-origin-driver
         state-get
         state-set/c
         state-merge
         state-empty
         state-origin
         state-get-state-set
         state-delete
         deref-config
         state-entry-config
         state-for-each)

(define STATE (make-parameter (hash 'serial 0 'resources (hash))))

(define (SERIAL-NO) (hash-ref (STATE) 'serial))
(define (RESOURCES) (hash-ref (STATE) 'resources))

(define (next-serial)
  (define next (+ 1 (SERIAL-NO)))
  (STATE (hash-set (STATE) 'serial next))
  next)

(struct state-origin (fingerprint destructor-cmd) #:prefab)
(struct state-entry (serial origin config) #:prefab)
(define state-empty (state-entry 0 (state-origin 0 null) (hash)))

(define (state-origin-driver origin)(hash-ref (state-origin-fingerprint origin) 'driver))

(define state-set/c (hash/c res-id/c state-entry? ))

(define (load-state f)
  (cond
    [ (file-exists? f)
      (with-input-from-file f (lambda() (STATE (read))))])
  (void))

(define (save-state f)
  (with-output-to-file f #:mode 'text #:exists 'replace
    (lambda () (write (STATE)))))

(define/contract (state-set-ref key origin config)
  (res-id/c state-origin? config/c . -> . void)
  (define resources (RESOURCES))
  (define serial
    (cond [(hash-has-key? resources key) (state-entry-serial(hash-ref resources key))]
          [else (next-serial) ]))
  (STATE (hash-set (STATE)
                   'resources (hash-set resources key (state-entry serial origin config))))
  void)

(define/contract (state-ref k)
  (res-id/c . -> . state-entry?)
  (hash-ref (RESOURCES) k))

(define/contract (state-ref-config k)
  (res-id/c . -> . config/c)
  (state-entry-config (hash-ref (RESOURCES) k)))

(define/contract (state-ref-origin k)
  (res-id/c . -> . state-origin?)
  (state-entry-origin (hash-ref (RESOURCES) k)))

(define (state-ref-serial k) (state-entry-serial (hash-ref (RESOURCES) k)))

(define state-ref-destructor-cmd (compose1 state-origin-destructor-cmd state-ref-origin))

(define (state-get) (STATE))
(define (state-keys) (hash-keys (RESOURCES)))

(define/contract (state-get-state-set)
  (-> state-set/c)
  (RESOURCES))
; (make-immutable-hash
;  (hash-map (RESOURCES) (lambda (k v) (cons k (state-entry-config v))))))

(define (state-for-each proc)
  (define (meta-def-proc k v) (proc k (state-entry-config v)))
  (hash-for-each (RESOURCES) meta-def-proc))

; TODO - just booms out
(define (resource-exists? m) (hash-ref (RESOURCES) m))

(define (state-delete m)
  (resource-exists? m)
  (STATE (hash-set (STATE) 'resources (hash-remove (RESOURCES) m))))

; (Deep) Merge a resource's config (rs) into an existing state, where rs overwrites st
(define/contract (state-merge st rs)
  (state-entry? resource/c . -> . state-entry?)

  (define (combine-hash s r)
    (hash-union s r #:combine merge-em))

  ; TODO41 - not sure if this is desired, e.g. can't empty a set of labels
  (define (merge-em s r)
    (cond [(and (hash? r) (hash? s)) (combine-hash s r)]
          [else r]))

  (define stc (state-entry-config st))
  (define rsc (resource-config rs))

  (define new-conf
    (if (eq? state-empty stc) rsc
        (combine-hash stc rsc)))
  (state-entry (state-entry-serial st) (state-entry-origin st) new-conf))

(define/contract (deref-config state cfg)
  (state-set/c config/c . -> . config/c)

  (define (get-ref ref)
    (define-values (res-id attr) (ref-split ref))
    (unpack-value (hash-nref (state-ref-config res-id) (id->list attr))))

  (define (deref-attr _ a)
    (update-val a (lambda (v) (if (ref? v) (get-ref v) v))))

  ; TODO41 - refactor to resource-update-config-fn
  (hash-apply cfg deref-attr))