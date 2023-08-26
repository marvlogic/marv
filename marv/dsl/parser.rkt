#lang brag

marv-spec: decl*
decl: var-decl | res-decl | for-each

var-decl: IDENTIFIER /"=" expression
expression: INTEGER | STRING | config-object | list
config-object: /"{" attr-decl* /"}"

config-expr: config-object | conf-ident | conf-merge
conf-merge: config-expr /"<-" config-expr
conf-ident: IDENTIFIER

attr-decl: IDENTIFIER /"=" (IDENTIFIER | expression)
list: /"[" expression* /"]"

res-decl: IDENTIFIER /"=" driver-id /":" driver-attr config-expr
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT

for-each: /"for/each" loop-var+ "{" marv-spec /"}"
loop-var: /"(" IDENTIFIER /"in" (list-ident | list) /")"
list-ident: IDENTIFIER