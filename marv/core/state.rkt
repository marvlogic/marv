#lang racket/base
(require racket/hash)
(require racket/contract)
(require marv/core/resources)

(provide load-state
         save-state
         state-keys
         state-set-ref
         state-ref
         state-ref-serial
         state-get
         state-merge
         mk-id->state
         state-delete
         state-for-each)

(define STATE (make-parameter (hash 'serial 0 'resources (hash))))

(define (SERIAL-NO) (hash-ref (STATE) 'serial))
(define (RESOURCES) (hash-ref (STATE) 'resources))

(define (next-serial)
  (define next (+ 1 (SERIAL-NO)))
  (STATE (hash-set (STATE) 'serial next))
  next)

(struct state-entry (serial resource) #:prefab)

(define (load-state f)
  (cond
    [ (file-exists? f)
      (with-input-from-file f (lambda() (STATE (read))))]))

(define (save-state f)
  (with-output-to-file f #:mode 'text #:exists 'replace
    (lambda () (write (STATE)))))

(define/contract (state-set-ref key res)
  (res-id/c resource/c . -> . void)
  (define resources (RESOURCES))
  (define serial
    (cond [(hash-has-key? resources key) (state-entry-serial(hash-ref resources key))]
          [else (next-serial) ]))
  (STATE (hash-set (STATE)
                   'resources (hash-set resources key (state-entry serial res))))
  void)

(define/contract (state-ref k)
  (res-id/c . -> . resource/c)
  (state-entry-resource (hash-ref (RESOURCES) k)))

(define (state-get) (STATE))
(define (state-keys) (hash-keys (RESOURCES)))

(define (state-ref-serial k) (state-entry-serial (hash-ref (RESOURCES) k)))

(define/contract (mk-id->state)
  (-> (hash/c res-id/c resource/c))
  (make-immutable-hash
   (hash-map (RESOURCES) (lambda (k v)
                           (cons k (state-entry-resource v))))))

(define (state-for-each proc)
  (define (meta-def-proc k v) (proc k (state-entry-resource v)))
  (hash-for-each (RESOURCES) meta-def-proc))

; TODO - just booms out
(define (resource-exists? m) (hash-ref (RESOURCES) m))

(define (state-delete m)
  (resource-exists? m)
  (STATE (hash-set (STATE) 'resources (hash-remove (RESOURCES) m))))

; (Deep) Merge a state record (rs) into an existing state, where rs overwrites st
(define/contract (state-merge st rs)
  (any/c resource/c . -> . resource/c)

  (define (combine-hash s r)
    (hash-union s r #:combine merge-em))

  (define (merge-em s r)
    (cond [(and (hash? r) (hash? s)) (combine-hash s r)]
          [else r]))

  (if (not st) rs
      (resource (resource-driver rs)
                (combine-hash (resource-config st) (resource-config rs)))))