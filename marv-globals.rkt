#lang racket/base

(require racket/file)
(require racket/list)
(require net/http-easy)

(provide marv-init
         workspace-path
         with-workspace-file)

(define WORKDIR (make-parameter (list (current-directory) ".marv")))

(define (marv-init) #f)

(define (workspace-path fs) (apply build-path (flatten (list (WORKDIR) fs))))

(define (with-workspace-file #:thunk thk #:url (url #f) . fpath)
  (define wspath (workspace-path fpath))
  (make-parent-directory* wspath)
  (when (and url (not (file-exists? wspath)))
    (printf "Downloading ~a \n  -> ~a\n" url wspath)
    (with-output-to-file wspath (lambda() (write-bytes (response-body (get url))))))
  (with-input-from-file wspath thk))