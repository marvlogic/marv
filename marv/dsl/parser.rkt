#lang brag

marv-spec: statement*

statement: decl | for-each
decl: var-decl | res-decl

comment: COMMENT
var-decl: IDENTIFIER /"=" expression
expression: INTEGER | STRING | config-object | list
config-object: /"{" attr-decl* /"}"
list: /"[" expression* /"]"

config-expr: config-object | conf-ident | conf-merge
conf-merge: config-expr /"<-" config-expr
conf-ident: IDENTIFIER

attr-decl: IDENTIFIER /"=" ( expression | IDENTIFIER )

res-decl: IDENTIFIER /"=" driver-id /":" driver-attr config-expr
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT

for-each: /"for/each" loop-var+ "{" marv-spec /"}"
loop-var: /"(" IDENTIFIER /"in" (list-ident | list) /")"
list-ident: IDENTIFIER