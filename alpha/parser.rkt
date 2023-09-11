#lang brag

marv-spec: statement*

statement: decl | pprint
decl: var-decl | res-decl | config-func-decl

pprint: /"pprint" expression
comment: COMMENT
var-decl: IDENTIFIER /"=" expression
config-func-decl: IDENTIFIER /"(" IDENTIFIER+ /")" /"=" config-expr
config-func-param: IDENTIFIER

expression: INTEGER | STRING | reference | config-func-call | boolean | config-expr | alist | built-in

boolean: "true" | "false"

config-object: /"{" attr-decl* /"}"
alist: /"[" expression* /"]"

list-attr: /"[" IDENTIFIER+ /"]"

built-in: env-read | strf
env-read: /"env" /"(" STRING /")"

strf: /"strf" /"(" STRING expression+ /")"

config-expr: config-object | config-ident | config-merge | config-func-call | config-take
config-merge: config-expr ("<-" | "->") config-expr
config-ident: IDENTIFIER
config-func-call: IDENTIFIER /"(" expression+ /")"
config-take: config-expr /"<<" list-attr

attr-decl: IDENTIFIER /"=" [ "imm:" ] ( expression | reference | IDENTIFIER)
reference: DOTTY-IDENT

res-decl: IDENTIFIER [ /"[" loop-ident /"]" ] /"=" driver-id /":" driver-attr config-expr
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT

loop-ident: IDENTIFIER

# for-list: /"for/list" loop-var "{" statement+ "}"
# loop-var: /"(" IDENTIFIER /"in" (list-ident | alist) /")"
# list-ident: IDENTIFIER