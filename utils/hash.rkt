#lang racket/base
(require racket/contract)
(require racket/function)
(require racket/hash)
(require racket/dict)

(provide hash-nref
         hash-nremove
         hash-remove-multi
         hash-merge
         hash-format-string
         dict-format-string
         hash->flatlist
         hash-apply
         make-immutable-caseless-string-hash
         hash-filter
         hash-keep
         hash-match?)

(define (hash-nref hs ks [def #f])
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)][else def]))])
    (for/fold ([h hs])
              ([k (in-list ks)])
      (hash-ref h k))))

(define (hash-nremove hs ks)
  (for/fold ([h hs])
            ([k (in-list ks)])
    (hash-remove h k)))

(define-syntax hash-remove-multi
  (syntax-rules ()
    [(hash-remove-multi h p) (hash-remove h p)]
    [(hash-remove-multi h p q ...) (hash-remove-multi (hash-remove h p) q ...)]
    ))

; Merge the two hashes, where the left has priority over the right
; NB for deep merging, see state-merge
(define (hash-merge left right) (hash-union left right #:combine (lambda (v0 _) v0)))

(define/contract (hash->flatlist hs)
  (hash? . -> . (listof any/c))

  (define (foldit lst acc key-prefix)
    (define (flat kv acc)
      (cond [(hash? (cdr kv)) (foldit (hash->list (cdr kv)) acc (format "~a." (car kv)))]
            [(list? (cdr kv))
             (foldit (for/list ([ix (in-range (length (cdr kv)))]
                                [v (cdr kv)]) (cons (format "~a.~a" (car kv) ix) v)) acc key-prefix)]
            [else (cons (cons (string->symbol(format "~a~a" key-prefix (car kv))) (cdr kv)) acc)]))

    (foldl flat acc lst))
  (foldit (hash->list hs) '() ""))

(define-custom-hash-types caseless-string-hash
  #:key? symbol?
  (lambda (x y) (equal? (string-upcase (symbol->string x)) (string-upcase (symbol->string y)))))

(define (hash-format-string hs str)
  (define var-regex #rx"\\{[^\\}]+\\}")
  (define (replace-for str)
    (format "~a" (hash-ref hs (string->symbol (substring str 1 (- (string-length str) 1))))))
  (regexp-replace* var-regex str replace-for))

(define (dict-format-string dct str)
  (define var-regex #rx"\\{[^\\}]+\\}")
  (define (replace-for str)
    (format "~a" (dict-ref dct (string->symbol (substring str 1 (- (string-length str) 1))))))
  (regexp-replace* var-regex str replace-for))

(define (hash-filter hs filter)
  (for/fold ([h hs]) ([k (in-list (hash-keys hs))])
    ( if (filter k (hash-ref hs k)) h (hash-remove h k))))

(define (hash-match? hs matchf)

  (define (domatch h)
    (define (sgl k val)
      (or (matchf k val)
          (cond [(hash? val) (domatch val)]
                [(list? val) (ormap (lambda(v)(sgl k v)) val)]
                [else #f])))
    (for/or ([key (in-list(hash-keys h))])
      (sgl key (hash-ref h key))))

  (domatch hs))

(define (hash-apply hs applyf)

  (define (inner k v)
    (define (sgl v)
      (cond [(hash? v) (hash-apply v applyf)]
            [(list? v) (map sgl v)]
            [else (applyf k v)]))
    (values k (sgl v)))

  (hash-map/copy hs inner))

(define (hash-keep hs ks)
  (for/hasheq ([i (in-list ks)] #:when (hash-has-key? hs i)) (values i (hash-ref hs i))))