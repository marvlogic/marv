#lang racket/base
(require racket/contract)
(require marv/core/resources)
(require marv/core/config)

(provide load-state
         save-state
         state-keys
         state-ref
         state-ref-config
         state-ref-origin
         state-ref-serial
         state-ref-destructor
         state-entry-origin
         state-entry-driver
         state-entry-config
         state-get
         state-set/c
         state-get-state-set
         state-set-ref
         state-delete
         state-for-each)

(define STATE (make-parameter (hash 'serial 0 'resources (hash))))

(define (SERIAL-NO) (hash-ref (STATE) 'serial))
(define (RESOURCES) (hash-ref (STATE) 'resources))

(define (next-serial)
  (define next (+ 1 (SERIAL-NO)))
  (STATE (hash-set (STATE) 'serial next))
  next)

(struct state-entry (serial origin destructor config) #:prefab)

(define state-set/c (hash/c res-id/c state-entry? ))

; TODO - set required fields in contract
(define state-destructor/c hash?)

(define (load-state f)
  (cond
    [ (file-exists? f)
      (with-input-from-file f (lambda() (STATE (read))))])
  (void))

(define (save-state f)
  (with-output-to-file f #:mode 'text #:exists 'replace
    (lambda () (write (STATE)))))

(define/contract (state-set-ref key origin destructor config)
  (res-id/c origin/c state-destructor/c config/c . -> . void)
  (define resources (RESOURCES))
  (define serial
    (cond [(hash-has-key? resources key) (state-entry-serial(hash-ref resources key))]
          [else (next-serial) ]))
  (STATE (hash-set (STATE)
                   'resources (hash-set resources key (state-entry serial origin destructor config))))
  void)

(define/contract (state-ref k)
  (res-id/c . -> . state-entry?)
  (hash-ref (RESOURCES) k))

(define (state-ref-serial k) (state-entry-serial (hash-ref (RESOURCES) k)))

(define/contract (state-ref-origin k)
  (res-id/c . -> . origin/c)
  (state-entry-origin (hash-ref (RESOURCES) k)))

(define/contract (state-ref-destructor k)
  (res-id/c . -> . state-destructor/c)
  (state-entry-destructor (hash-ref (RESOURCES) k)))

(define/contract (state-ref-config k)
  (res-id/c . -> . config/c)
  (state-entry-config (hash-ref (RESOURCES) k)))

(define (state-entry-driver st)
  (hash-ref (state-entry-origin st) 'driver))

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