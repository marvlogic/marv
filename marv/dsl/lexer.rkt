#lang racket/base

(require brag/support)

(provide basic-lexer)

(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev identifier (:seq (:= 1 alphabetic)
                                    (:* (:or digits alphabetic))))
(define-lex-abbrev dotty-ident (:seq (:= 1 identifier) (:+ (:seq "." identifier))))

(define basic-lexer
  (lexer-srcloc
   ;    ["\n" (token 'NEWLINE lexeme)]
   [whitespace (token lexeme #:skip? #t)]
   [dotty-ident (token 'DOTTY-IDENT lexeme) ]
   [identifier (token 'IDENTIFIER lexeme) ]
   [(:= 1 (char-set "{}=:")) lexeme]
   [digits (token 'INTEGER (string->number lexeme))]
   ;    [(from/stop-before "rem" "\n") (token 'REM lexeme)]
   ;    [(:or "print" "goto" "end"
   ;  "+" ":" ";") (token lexeme lexeme)]
   ;    [(:or (:seq (:? digits) "." digits)
   ;  (:seq digits "."))
   ; (token 'DECIMAL (string->number lexeme))]
   [(:or (from/to "\"" "\"") (from/to "'" "'"))
    (token 'STRING
           (substring lexeme
                      1 (sub1 (string-length lexeme))))]))
