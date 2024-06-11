# Introduction

`marv` is an Infrastructure-as-Code tool for declaring and managing cloud
resources.

You define your infrastructure in files using Marv's DSL, and then apply the
definition which creates/updates/deletes the actual resources. `marv` manages
the lifecycle of the resources to keep the infrastructure in sync with code.

# Status

__Under active development: Not suitable for production usage__

>The code is emerging from its "prototype phase", which is to say that while it
is much more stable and usable, there remains a lot of refactoring and
improvements to make.

## Developer's Update - June 2024

Issue #41 (type system development) has been in progress since December, though
there was a 3 month break while I was working for a client on another project. 

The type system took a *lot* of work, and several dead-ends, before I settled on
something that will be a solid foundation to build on. 

I'm also happy that the new type system and language features are flexible
enough to cope with the quirks of the various GCP APIs; there should be little
or no need to resort to changing the Racket-based core of Marv itself to work
around issues in APIs.

It's now far easier to create custom resource types in Marv, though it is still
limited to Google Cloud Platform.

## Features so far...

- GCP API support for:
  - Compute 
  - Storage
  - IAM (partial)
  - Secret Manager
- DSL for describing resources ([tutorial](docs/tutorial/01-bucket-example.md))
- Modules, including parameters and returns (outputs)
- Custom types (for accessing parts of the API not covered in the standard model)
- Local state file

You could play with this locally in your own GCP projects etc. It might be
useful for managing small development environments and you would have to manage
the state file (ie keep it safe).

## Current Limitations

There's a lot of work left to reach a good beta release point. The main issues
and omissions are:

- A new DSL subject to change, and lacking in some features such as loops and
conditions. All are planned though.

- The model doesn't yet know about which fields are immutable on GCP resources; you
need to specify these manually (see attributes marked `imm:` in the
[examples](examples/gcp/shared/network-base.mrv)).

- Bare-bones error checking, and error reports are often obtuse. A lot of times
errors aren't caught until GCP sees the configuration during an `apply` phase. 

- Only supports local state files
  
# Documentation

 See https://marvlogic.github.io/marv/

# Marv's DSL (marv-speak)

There's a [tutorial](docs/tutorial/01-bucket-example.md).

You could also look at the [first example](examples/gcp/01-networking.mrv), and
follow the trail from there into the [network-base
module](examples/gcp/shared/network-base.mrv).

The marv language specification is written in
[brag](https://docs.racket-lang.org/brag/index.html) and is [defined
here](alpha/parser.rkt).

# Notes

## Build a standalone
(deprecated)

    raco exe ++lib net/http-easy ++lang racket/base --collects-path ./ -o marv command.rkt()