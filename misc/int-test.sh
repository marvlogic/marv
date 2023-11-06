#!/bin/bash

STATE=int-test-state.dat
RESF=examples/gcp/01-networking.mrv

export MARV_DEV_DRIVER=y
export MARV_GCP_PROJECT=nosuchproject
export MARV_GCP_PROJECT=europe-west1

rm $STATE

marv() {
    racket command.rkt $*
}

set -e

# racket unittests.rkt

echo "Planning"
echo "========"
marv --plan $RESF
echo "Executing"
echo "========="
marv --apply $RESF
echo "Planning again"
echo "=============="
marv --plan $RESF
# exit
marv --dump $RESF

echo "*** REMOVING ***"
marv --plan --purge $RESF 
marv --apply --purge $RESF 
marv --dump $RESF 