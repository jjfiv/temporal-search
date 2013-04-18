#!/bin/sh

TMP_BIN=/tmp/bin

# meant to be invoked from root of the project
set -e
rm -rf $TMP_BIN
mkdir $TMP_BIN

mvn -q package

cp target/temporal-search*.jar $TMP_BIN
cp -R target/lib $TMP_BIN

rsync -r --partial -h --progress $TMP_BIN swarm.cs.umass.edu:/work1/allan/jfoley/ts/

