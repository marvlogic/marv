#lang brag

marv-spec: marv-module*

marv-module: /"module" IDENTIFIER ["(" module-parameter+ ")"] /"{" statement* /"}"
module-parameter: IDENTIFIER

statement: decl | pprint
decl: var-decl | res-decl | module-invoke | config-func-decl | type-decl

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

; type is explicitly allowed as it's so common in configs, and we need 'type' as a lexical token;
; also allow STRING to allow user to avoid marv keywords
attr-decl: ( STRING | IDENTIFIER | "type" ) /"=" [ "imm:" ] expression

reference: DOTTY-IDENT

res-decl: IDENTIFIER /"=" driver-id /":" driver-attr config-expr
module-invoke: IDENTIFIER /"=" IDENTIFIER /"(" named-parameter+ [ /"," ] /")"

; type is explicitly allowed as it's common, and we need 'type' as a lexical token
; also allow STRING to allow user to avoid marv keywords
named-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

driver-id: IDENTIFIER
driver-attr: DOTTY-IDENT