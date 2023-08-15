#!/bin/bash

STATE=int-test-state.dat
RESF=int-test-resources.rkt

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
marv --plan --param skip-define YES $RESF 
marv --apply --param skip-define YES $RESF 
marv --dump --param skip-define YES $RESF 