
# Introduction

`marv` is a DevOps tool designed to help people manage their Infrastructure as Code.

It allows you to define your infrastructure in files, and then apply the
definition which creates/updates/deletes the actual resources. `marv` manages
the lifecycle of the resources to keep the infrastructure in sync with code.

# Status

__Barely usable:  only suitable for tinkering or non-prod situations__

## __Current__ Limitations

Tons of them! Rest assured I'll be working to resolve these and many others.

The most important ones:

- No DSL; you have to declare your resources using Racket structures.

- Only supports GCP compute and storage APIs.

- Barely any error checking, most of the time the only error checking is
performed by GCP itself during an `apply` phase.

- Example isn't documented enough.

- Only a local state file is supported.

## Excuses

This is my first major project using Racket, so the structure of it is very much
in-flux and subject to __significant__ change. 

It's also been a learning experience in Racket programming, so there are a lot
of areas in code that are, er - shall we say, /a bit on the rough side/. 

That said, it was meant as a prototype to learn how to implement an IAC
management system: inevitably, large chunks will get re-written.


# Installation

Install Racket from: https://download.racket-lang.org

NB not sure about Racket dependencies as yet. Might be:

    raco pkg install graph-lib  \
        http-easy-lib     \
        racket-langserver \
        sha               \
        yaml              \
        zstd               

# Running the example

    # GCP project must already exist
    export MARV_GCP_PROJECT=...
    export MARV_GCP_REGION=europe-west2
    export GCP_ACCESS_TOKEN=`gcloud auth print-access-token`

    alias marv="racket command.rkt"
    marv --plan examples/gcp/load-balancer.rkt 
    marv --apply examples/gcp/load-balancer.rkt 

    # Delete all resources
    PURGE=y marv --apply examples/gcp/load-balancer.rkt 

The state is stored locally, in 'state.dat' by default. Use `-s` to change this.

Cached information is held in the `.marv` directory - this is mainly for holding
downloaded GCP API schema information.

# Notes

## Build a standalone

    raco exe ++lib net/http-easy ++lang racket/base --collects-path ./ -o marv command.rkt