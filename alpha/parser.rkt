#lang brag

marv-spec: module-import* outer-decl* marv-module*

module-import: /"import" [ STRING | MODULE-IDENTIFIER ] [ "as" IDENTIFIER ]

outer-decl: func-decl | type-decl | type-template | var-decl | module-export

module-export: /"export" [ IDENTIFIER+ [ "as" IDENTIFIER ] ]+

marv-module: [ "private" ] /"module" IDENTIFIER ["(" module-parameter+ ")"] /"{" statement* [ module-return ] /"}"
module-parameter: IDENTIFIER [ "=" expression ]
module-return: "return" /"{" return-parameter+ /"}"
return-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

statement: decl | pprint
decl: var-decl | res-decl | module-invoke | func-decl

pprint: /"pprint" expression
comment: COMMENT
var-decl: IDENTIFIER /"=" expression

func-decl: IDENTIFIER /"(" (IDENTIFIER [ /"," ])* /")" /"=" expression
func-call: func-ident /"(" (expression [ /"," ])* /")"
func-ident: (DOTTY-IDENT | IDENTIFIER)

verb: IDENTIFIER
type-id: IDENTIFIER
transformer-id: IDENTIFIER
api-id: DOTTY-IDENT

expression: INTEGER | STRING | IDENTIFIER | func-call | boolean | config-expr | reference | alist | built-in

string-expression: STRING | expression

boolean: "true" | "false"

attribute-name: ( STRING | IDENTIFIER | "type" )

config-object: /"{" [( STRING | IDENTIFIER | "type" ) /"=" [ "imm:" ] expression [ /"," ]]* /"}"
alist: /"[" expression* /"]"

list-attr: /"[" ( attribute-name [ /"," ] )* /"]"

built-in: env-read | strf | base64encode | base64decode | urivars | uritemplate
env-read: /"env" /"(" STRING /")"

strf: /"strf" /"(" STRING ( expression [ /"," ] ) + /")"
urivars: /"strvars" /"(" expression /")"

uritemplate: /"expandvars" /"(" expression config-expr /")"
base64encode: /"base64encode" /"(" expression /")"
base64decode: /"base64decode" /"(" expression /")"

config-expr: reference | config-object | config-ident | config-merge | config-take | func-call
config-merge: config-expr ("<-" | "->") config-expr
config-ident: IDENTIFIER
config-take: config-expr /"<<" ( urivars | list-attr )

reference: DOTTY-IDENT

res-decl: IDENTIFIER /"=" type-id config-expr
module-invoke: IDENTIFIER /"=" ( MODULE-IDENTIFIER | IDENTIFIER ) /"(" (named-parameter [ /"," ] )* /")"

; type is explicitly allowed as it's common, and we need 'type' as a lexical token
; also allow STRING to allow user to avoid marv keywords
named-parameter: ( STRING | IDENTIFIER | "type" ) /"=" expression

type-parameters: type-id /"<" ( IDENTIFIER [/","] )+ /">"
type-template: /"type" type-parameters /"=" /"{" func-decl+ [ type-wild ]* /"}"

type-decl: /"type" type-id /"=" ( /"{" func-decl+ [ type-wild ]* /"}" | type-parameters )
type-wild: /"*" /"=" IDENTIFIER /"." /"*"