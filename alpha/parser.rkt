#lang brag

marv-spec: marv-module*

marv-module: /"module" IDENTIFIER ["(" parameters+ ")"] /"{" statement* /"}"

parameters: IDENTIFIER

statement: decl | pprint
decl: var-decl | res-decl | config-func-decl | type-decl

pprint: /"pprint" expression
comment: COMMENT
var-decl: IDENTIFIER /"=" expression

config-func-decl: IDENTIFIER /"(" IDENTIFIER+ /")" /"=" config-expr
config-func-param: IDENTIFIER

type-decl: /"type" driver-id /":" driver-attr /"=" type-body
type-body: /"{" type-crud-decl+ /"}"
type-crud-decl: ( "create" | "delete" ) /"=" type-api-spec
type-api-spec: driver-attr /":" IDENTIFIER

expression: INTEGER | STRING | IDENTIFIER | reference | config-func-call | boolean | config-expr | alist | built-in

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

; type is explicitly allowed as it's so common in configs
attr-decl: ( IDENTIFIER | "type" | STRING ) /"=" [ "imm:" ] expression
reference: DOTTY-IDENT

res-decl: IDENTIFIER /"=" driver-id /":" driver-attr config-expr
driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT

; loop-ident: IDENTIFIER
# for-list: /"for/list" loop-var "{" statement+ "}"
# loop-var: /"(" IDENTIFIER /"in" (list-ident | alist) /")"
# list-ident: IDENTIFIER