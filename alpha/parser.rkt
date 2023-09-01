#lang brag

marv-spec: statement*

statement: decl | for-list | pprint
decl: var-decl | res-decl | conf-func-decl

pprint: /"pprint" expression
comment: COMMENT
var-decl: IDENTIFIER /"=" expression
conf-func-decl: IDENTIFIER /"(" IDENTIFIER+ /")" /"=" config-expr
conf-func-param: IDENTIFIER

expression: INTEGER | STRING | reference | conf-func-call | boolean | config-expr | alist | built-in

boolean: "true" | "false"

config-object: /"{" attr-decl* /"}"
alist: /"[" expression* /"]"

conf-func-call: IDENTIFIER /"(" expression+ /")"

built-in: env-read
env-read: /"env" /"(" STRING /")"

config-expr: config-object | config-ident | config-merge | conf-func-call
config-merge: config-expr ("<-" | "->") config-expr
config-ident: IDENTIFIER

attr-decl: IDENTIFIER /"=" [ "imm:" ] ( expression | reference | IDENTIFIER)
reference: DOTTY-IDENT

res-decl: IDENTIFIER [ /"[" loop-ident /"]" ] /"=" driver-id /":" driver-attr config-expr
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT

loop-ident: IDENTIFIER

for-list: /"for/list" loop-var "{" statement+ "}"
loop-var: /"(" IDENTIFIER /"in" (list-ident | alist) /")"
list-ident: IDENTIFIER