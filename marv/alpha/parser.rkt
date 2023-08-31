#lang brag

marv-spec: statement*

statement: decl | for-list | pprint
decl: var-decl | res-decl

pprint: /"pprint" IDENTIFIER
comment: COMMENT
var-decl: IDENTIFIER /"=" expression
expression: INTEGER | STRING | boolean | config-expr | alist | built-in

boolean: "true" | "false"

config-object: /"{" attr-decl* /"}"
alist: /"[" expression* /"]"

built-in: env-read
env-read: /"env" /"(" STRING /")"

config-expr: config-object | config-ident | config-merge
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