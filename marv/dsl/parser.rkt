#lang brag

marv-spec: statement*

statement: decl | for-each
decl: var-decl | res-decl

comment: COMMENT
var-decl: IDENTIFIER /"=" expression
expression: INTEGER | STRING | boolean | config-expr | list | built-in

boolean: "true" | "false"

config-object: /"{" attr-decl* /"}"
list: /"[" expression* /"]"

built-in: env-read
env-read: /"env" /"(" STRING /")"

config-expr: config-object | config-ident | config-merge
config-merge: config-expr /"<-" config-expr
config-ident: IDENTIFIER

attr-decl: IDENTIFIER /"=" ( expression | reference | IDENTIFIER)
reference: DOTTY-IDENT

res-decl: IDENTIFIER /"=" driver-id /":" driver-attr config-expr
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT

for-each: /"for/each" loop-var+ "{" marv-spec /"}"
loop-var: /"(" IDENTIFIER /"in" (list-ident | list) /")"
list-ident: IDENTIFIER