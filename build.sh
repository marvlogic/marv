#!/bin/bash

TAG=${1:-latest}
BUILD=`mktemp -d`
echo "Building in $BUILD"

cp Dockerfile marv.sh $BUILD
tar zcf $BUILD/marv.tgz alpha/ command.rkt core/ drivers/ examples/ info.rkt main.rkt utils/
docker build -t happyrat/marv:$TAG $BUILD
rm -rf $BUILD