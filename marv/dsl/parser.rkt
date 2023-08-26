#lang brag

marv-spec: decl*
decl: var-decl | res-decl

var-decl: IDENTIFIER /"=" expression
expression: INTEGER | STRING | config_object
config_object: /"{" attr-decl* /"}"
attr-decl: IDENTIFIER /"=" expression

res-decl: IDENTIFIER /"=" driver-id /":" driver-attr config_object
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT