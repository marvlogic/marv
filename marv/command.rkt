#lang racket/base

(require racket/cmdline)
(require racket/pretty)
(require racket/string)
(require racket/path)

(require marv/core/globals)
(require marv/core/state)
(require marv/core/resources)
(require marv/core/resource-def)
(require marv/core/lifecycle)

(define APPLY (make-parameter #f))
(define PURGE (make-parameter #f))
(define SHOW-STATE-IDS (make-parameter #f))
(define DUMP-RESOURCES (make-parameter #f))
(define LIST-RESOURCES (make-parameter #f))
(define REFRESH-STATE (make-parameter #f))
(define IMPORT-RESOURCES (make-parameter #f))
(define DIFF-STATE-REMOTE (make-parameter #f))
(define DIFF-SPEC-STATE (make-parameter #f))
(define PLAN-CHANGES (make-parameter #f))
(define STATE-RM (make-parameter #f))
(define LIST-PARAMS (make-parameter #f))
(define PARAMS (make-parameter (hash)))

(define STATE-FILE (make-parameter #f))

(define RESOURCES
  (command-line
   #:once-each
   [("-s" "--state") state-file "Name of statefile to use" (STATE-FILE state-file)]
   [("--purge")  "Purge (DELETE) all resources" (PURGE #t)]

   #:once-any
   [("--plan")  "Plan changes" (PLAN-CHANGES #t)]
   [("--apply")  "Apply resources" (APPLY #t)]
   [("--list") "Show the defined resources" (LIST-RESOURCES #t)]
   [("--state-ids") "List resource IDs from state" (SHOW-STATE-IDS #t)]
   [("--dump") "Dump full output of resources from state" (DUMP-RESOURCES #t)]
   ;  [("--refresh") "Refresh the state" (REFRESH-STATE #t)]
   [("--import") ids "Import the already existing resource IDs into state" (IMPORT-RESOURCES ids)]
   ;  [("--remote-diff") "Report difference between STATE & remote" (DIFF-STATE-REMOTE #t)]
   [("--state-rm") id "Remove item from state" (STATE-RM id)]
   [("--list-params") "Lists parameters accepted by module" (LIST-PARAMS #t)]

   #:multi
   [("--param") param value "Set <param> to <value>" (PARAMS (hash-set (PARAMS) param value))]

   #:args (module-file) module-file))

(marv-init)

(when (not (STATE-FILE))
  (STATE-FILE (path-replace-extension (file-name-from-path (string->path RESOURCES)) ".state.dat" )))

(printf "Using state file: ~a\n" (STATE-FILE))
(load-state (STATE-FILE))

(init-module RESOURCES (PURGE))

(define modl (get-module (PARAMS)))

(cond
  [(SHOW-STATE-IDS)
   (pretty-print (state-keys))]

  ; [(DIFF-STATE-REMOTE)
  ;  (define (do-diff k v)
  ;    ; TODO: will fail if remote is removed
  ;    (define remote-state (drv-read #:meta k #:resource v))
  ;    (pretty-display(make-diff v remote-state)))
  ;  (state-for-each do-diff)]

  ; [(REFRESH-STATE)
  ;  (define (update k)
  ;    (state-set-ref
  ;     k
  ;     (drv-read #:meta k
  ;               #:resource (state-ref k) )))
  ;  (map update (state-keys))
  ;  ]

  [(IMPORT-RESOURCES)
   (import-resources modl (map string->symbol (string-split (IMPORT-RESOURCES) ",")))]

  [(DUMP-RESOURCES) (pretty-print(state-get))]

  [(LIST-RESOURCES) (pretty-print (rmodule-resources modl))]

  [(PLAN-CHANGES)
   (define plan (plan-changes modl))
   (output-plan plan)]

  [(APPLY) (call-with-exception-handler
            (lambda(e)
              (printf "\nERROR: ~a\n" e)
              (save-state (STATE-FILE))
              (exit 1))
            (lambda() (apply-changes modl)))]

  [(STATE-RM)
   (define ids (map string->symbol (string-split (STATE-RM) ",")))
   (map (lambda (id) (printf "deleting ~a" id) (state-delete id)) ids)]


  [(LIST-PARAMS)
   (define params (get-resource-params))
   (displayln (params-accepted params))
   (displayln (params-required params))
   ]
  )

(cond
  [(or (IMPORT-RESOURCES) (REFRESH-STATE) (APPLY) (STATE-RM))
   (save-state (STATE-FILE)) ])