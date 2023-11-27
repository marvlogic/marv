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

# Running the example

    # GCP project must already exist
    export MARV_GCP_PROJECT=...
    export MARV_GCP_REGION=europe-west1

    # Take care that you refresh the token regularly - it expires after 1 hour
    # and may leave a partial-application if it expires during a run
    
    export GCP_ACCESS_TOKEN=`gcloud auth print-access-token`

    alias marv="racket command.rkt"

    # NB '--plan' doesn't actually output a plan-file, it just previews the changes.
    marv --plan examples/gcp/01-networking.mrv
    marv --apply examples/gcp/01-networking.mrv

    # Delete all resources
    marv --purge --apply examples/gcp/01-networking.mrv

The state is stored locally in `01-networking.state.dat` by default. Use `-s` to
override this.

Cached information is held in the `.marv` directory - this is mainly for holding
downloaded GCP API schema information.
