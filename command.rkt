#lang racket/base

(require racket/cmdline)
(require racket/pretty)
(require racket/string)

(require "marv-globals.rkt")
(require "state.rkt")
(require "diff.rkt")
(require "resources.rkt")
(require "resource-def.rkt")
(require "lifecycle.rkt")
(require "hash-utils.rkt")

(define APPLY (make-parameter #f))
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

(define STATE-FILE (make-parameter "state.dat"))

(define RESOURCES
  (command-line
   #:once-each
   [("-s" "--state") state-file "Name of statefile to use" (STATE-FILE state-file)]
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

(load-state (STATE-FILE))

(init-module RESOURCES)

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
   (map
    (lambda (op) (displayln (format "~a -> ~a"  (car op) (cdr op))))
    (plan-ordered-operations plan))]

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