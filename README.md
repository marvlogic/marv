# Introduction

`marv` is a DevOps tool designed to help people manage their Infrastructure as Code.

It allows you to define your infrastructure in files, and then apply the
definition which creates/updates/deletes the actual resources. `marv` manages
the lifecycle of the resources to keep the infrastructure in sync with code.

# Status

__Under active development: Not suitable for production usage__

You could play with this locally in your own GCP projects etc. It might be
useful for managing small development environments. You would have to manage the
state file manually.

## Current Limitations

Tons of them! Rest assured I'll be working to resolve these and many others.

The most important ones:

- A new DSL, in alpha quality. Subject to change. Lacking in features, such as
loops.

- Only supports GCP compute and storage APIs.

- Barely any error checking, most of the time the only error checking is
performed by GCP itself during an `apply` phase.

- The [Racket example](examples/gcp/load-balancer.rkt) isn't documented enough, and hard-coded to `europe-west2`

- Only supports local state files

## Excuses

This is my first major project using Racket, so the structure of it is very much
in-flux and subject to __significant__ change. Its a bit of a prototype &
learning platform as much as anything else.

# Installation

Install Racket from: https://download.racket-lang.org

Install `marv` using `raco`:

    # From the root of the cloned project:
    raco pkg install

# Marv's DSL (marv-speak)

Have a read of the [example](examples/gcp/load-balancer.mrv), which is fairly
well documented.

The detailed marv language specification is written in
[brag](https://docs.racket-lang.org/brag/index.html) and is [defined here](alpha/parser.rkt).

As well as Marv-speak, you can also declare your [resources in Racket](examples/gcp/load-balancer.rkt).

The two examples are interchangeable - they declare the exact same resources, so
you can `marv --apply` one or the other and they will create the same GCP
resources. 

The only difference is that the `mrv` file doesn't accept parameters via the
command line (for now).

# Running the example

    # GCP project must already exist
    export MARV_GCP_PROJECT=...
    export MARV_GCP_REGION=europe-west2

    # Take care that you refresh the token regularly - it expires after 1 hour
    # and may leave a partial-application if it expires during a run
    
    export GCP_ACCESS_TOKEN=`gcloud auth print-access-token`

    alias marv="racket command.rkt"
    marv --plan examples/gcp/load-balancer.mrv
    marv --apply examples/gcp/load-balancer.mrv 

    # Delete all resources
    marv --purge --apply examples/gcp/load-balancer.mrv 

The state is stored locally in `load-balancer.state.dat` by default. Use `-s` to
override this.

Cached information is held in the `.marv` directory - this is mainly for holding
downloaded GCP API schema information.

# Notes

## Build a standalone(deprecated)

    raco exe ++lib net/http-easy ++lang racket/base --collects-path ./ -o marv command.rkt