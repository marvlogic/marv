#!/bin/bash

STATE=int-test-state.dat
RESF=int-test-resources.rkt

rm $STATE

source iac2.inc

set -e

# racket unittests.rkt

echo "Planning"
echo "========"
iac2 plan $RESF
echo "Executing"
echo "========="
iac2 apply $RESF
echo "Planning again"
echo "=============="
iac2 plan $RESF
# exit
iac2 dump $RESF

echo "*** REMOVING ***"
iac2 plan $RESF --param skip-define YES
iac2 apply $RESF --param skip-define YES
iac2 dump $RESF --param skip-define YES