#lang racket/base

(require rackunit "hash-utils.rkt")

(define driver1 #hasheq((project . "happy1") (region . "euwest1")))
(define config1 #hasheq((project . "happy2") (region . "euwest2")))
(define config2 #hasheq((region . "russia")))
(define config3 #hasheq())
(define config4 #hasheq((project . "happy2")
                        (region . "euwest2")
                        (nested . #hasheq( (replace-me . "goob-boy")))))

'hash-utils-test

(check-equal? (hash-nref config4 '(project)) "happy2")
(check-equal? (hash-nref config4 '(nested replace-me)) "goob-boy")

(check-equal? (hash-nremove config1 '(project region)) #hasheq())
(check-equal? (hash-merge config1 driver1) config1)

(check-equal? (hash-merge config2 driver1)
              #hasheq(( project . "happy1") (region . "russia")))

(check-equal? (hash-merge config3 driver1)
              #hasheq(( project . "happy1") (region . "euwest1")))

(check-equal? (hash-merge config3 driver1 )
              #hasheq(( project . "happy1") (region . "euwest1")))

(check-equal? (hash-format-string config1 "{project}/{region}") "happy2/euwest2")

(check-true (hash-match? #hasheq((nested . #hasheq((region . "euwest1"))))
                         (lambda (k v) (and (eq? 'nested k) (hash? v)))))

(check-true (hash-match? #hasheq((nested . #hasheq((region . "euwest1"))))
                         (lambda (k v) (eq? "euwest1" v))))
(check-true (hash-match? #hasheq((nested . #hasheq((region . "euwest1"))))
                         (lambda (k v) (eq? 'region k))))
(check-false (hash-match? #hasheq((nested . #hasheq((region . "euwest1"))))
                          (lambda (k v) (eq? "euwest" v))))
(check-false (hash-match? #hasheq((nested . #hasheq((region1 . "euwest1"))))
                          (lambda (k v) (eq? 'region k))))

(check-equal? (hash-apply config4 (lambda (k v) (if (eq? k 'replace-me) "replaced" v)))
              (hash-set config4 'nested (hasheq 'replace-me "replaced")))

(check-equal? (hash-apply config4 (lambda (k v) (if (eq? k 'region) "replaced" v)))
              (hash-set config4 'region "replaced"))

(check-equal? (hash-filter config1 (lambda (k v) (eq? 'project k))) #hasheq((project . "happy2")))
(check-equal? (hash-filter config1 (lambda (k v) (equal? "euwest2" v))) #hasheq((region . "euwest2")))

(check-equal? (hash->flatlist #hasheq((x . y)(y . #hasheq((z . "hello"))))) '((x . y) (y.z . "hello")))

(define fl-test #hasheq(
                 (f0 . kevin)
                 (f1 . #hasheq((line1 . "77 Shadow")))
                 (f2 . (#hasheq((line2 . "66 Shadow")) x y))
                 (f3 . #hasheq((line3 . (a b c))))))

(define (pair-sort lst) (sort lst (lambda(x y)(symbol<? (car x) (car y) ))))

(check-equal?
 (pair-sort (hash->flatlist fl-test))
 (pair-sort
  '( (f0 . kevin)
     (f1.line1 . "77 Shadow")
     (f2.0.line2 . "66 Shadow")
     (f2.1 . x)
     (f2.2 . y)
     (f3.line3.0 . a )
     (f3.line3.1 . b )
     (f3.line3.2 . c )))
 )