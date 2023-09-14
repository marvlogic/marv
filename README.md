# Introduction

`marv` is a DevOps tool designed to help people manage their Infrastructure as Code.

It allows you to define your infrastructure in files, and then apply the
definition which creates/updates/deletes the actual resources. `marv` manages
the lifecycle of the resources to keep the infrastructure in sync with code.

# Status

__Under active development: Not suitable for production usage__

You could play with this locally in your own GCP projects etc. It might be
useful for managing small development environments. You would have to manage the
state file locally.

The code is not in the best of places - consider it very 'prototypey' :)
(though it is improving by the day)

This is my first major project using Racket, which I'm still learning, so the
structure of it is very much in-flux and subject to __significant__ change. 

## Current Limitations

There's a lot of work left to reach a good point to release it. There are a load
of features that need to be added.

The most important ones are:

- A new DSL, in alpha quality. Subject to change. Lacking in features, such as
loops, modules and parameters. All are planned though.

- The model doesn't know about which fields are immutable on GCP resources; you
need to specify these manually (see attributes marked `imm:` in the
[example](examples/gcp/load-balancer.mrv)).

- Only supports GCP compute and storage APIs, more coming very soon.

- Barely any error checking, most of the time the only error checking is
performed by GCP itself during an `apply` phase. Parser errors are likely to be
obtuse.

- Only supports local state files

# Installation - Linux & Mac

Install Racket from: https://download.racket-lang.org

Install `marv` using `raco`:

    # From the root of the cloned project:
    raco pkg install

# Docker Image

Marv is available as a pre-built docker image:

    docker pull happyrat/marv:latest

The image contains the [Google cloud SDK](https://cloud.google.com/sdk) i.e the
`gcloud` command is available. You will need to `gcloud auth login` from inside
the container, or arrange authentication via some other method.

To build the image locally:

    ./build.sh [TAG]  # TAG defaults to 'latest'

`marv` is available as an installed command when running in the container e.g.:

    marv --plan /usr/lib/marv/examples/gcp/load-balancer.mrv

etc...

# Command line usage
```
alias marv="racket command.rkt" # (not needed in docker container)
marv -h
usage: command.rkt [ <option> ... ] <module-file>

<option> is one of

  -s <state-file>, --state <state-file>
     Name of statefile to use
  --purge
     Purge (DELETE) all resources
/ --plan
|    Plan changes
| --apply
|    Apply resources
| --list
|    Show the defined resources
| --state-ids
|    List resource IDs from state
| --dump
|    Dump full output of resources from state
| --import <ids>
|    Import the already existing resource IDs into state
| --state-rm <id>
|    Remove item from state
| --list-params
\    Lists parameters accepted by module
* --param <param> <value>
     Set <param> to <value>
  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 *   Asterisks indicate options allowed multiple times.
 /|\ Brackets indicate mutually exclusive options.

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
 ```
    
# Marv's DSL (marv-speak)

Have a read of the [example](examples/gcp/load-balancer.mrv), which is fairly
well documented.

The detailed marv language specification is written in
[brag](https://docs.racket-lang.org/brag/index.html) and is [defined here](alpha/parser.rkt).

You can't pass parameters into the example, because the DSL lacks support for
this at the moment.

NB the example relies on the europe-west1 region; you need to change the
environment variable and the example file itself to use another region.

# Running the example

    # GCP project must already exist
    export MARV_GCP_PROJECT=...

    # Note that this is dependent on the value in example's instance-group-manager, if
    # you change this then change the example's value as well
    export MARV_GCP_REGION=europe-west1

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