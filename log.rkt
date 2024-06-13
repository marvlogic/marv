#lang racket/base

(require racket/pretty)

(provide log-marv-debug log-marv-warn log-marv-error log-marv-info log-equal?)

(define (get-fn lvl)
  (define levels
    (hash
     'debug pretty-display
     'info pretty-display
     'warn pretty-display
     'error pretty-display
     )
    )
  (if (getenv "MARV_LOG")
      (hash-ref levels lvl) void))

(define (log-marv-debug fstr . vs)
  ((get-fn 'debug) (apply format (string-append "DEBUG: " fstr) vs)))

(define (log-marv-info fstr . vs)
  ((get-fn 'info) (apply format (string-append "INFO: " fstr) vs)))

(define (log-marv-warn fstr . vs)
  ((get-fn 'warn) (apply format (string-append "WARN: " fstr) vs)))

(define (log-marv-error fstr . vs)
  ((get-fn 'error) (apply format (string-append "ERROR: " fstr) vs)))

(define (log-equal? a b) (log-marv-debug "equal?: ~a ~a" a b) (equal? a b))
