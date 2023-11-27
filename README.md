# Introduction

`marv` is an Infrastructure-as-Code tool for declaring and managing cloud
resources.

You define your infrastructure in files using Marv's DSL, and then apply the
definition which creates/updates/deletes the actual resources. `marv` manages
the lifecycle of the resources to keep the infrastructure in sync with code.

![Demo of Marv CLI](screencap.gif)

# Status

__Under active development: Not suitable for production usage__

The code is emerging from its "prototype phase", which is to say that there
remains a lot of refactoring and improvments to make.

## Features so far...

- Support for GCP Compute, Storage and (partial) IAM APIs (more planned soon)
- New DSL for describing resources
- Modules, including parameters and returns (outputs)
- Custom types (for accessing parts of the API not covered in the standard model)
- Local state file

You could play with this locally in your own GCP projects etc. It might be
useful for managing small development environments; you would have to manage the
state file locally.

## Current Limitations

There's a lot of work left to reach a good beta release point. The main issues
and omissions are:

- A new DSL subject to change, and lacking in some features such as loops and
conditions. All are planned though.

- GCP API support for:
  - Compute 
  - Storage
  - IAM (partial)
  - Secret Manager

- The model doesn't yet know about which fields are immutable on GCP resources; you
need to specify these manually (see attributes marked `imm:` in the
[examples](examples/gcp/shared/network-base.mrv)).

- Bare-bones error checking, and error reports are often obtuse. A lot of times
errors aren't caught until GCP sees the configuration during an `apply` phase. 

- Only supports local state files

    
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

    raco exe ++lib net/http-easy ++lang racket/base --collects-path ./ -o marv command.rkt