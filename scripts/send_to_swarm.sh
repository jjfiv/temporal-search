#!/bin/sh

# meant to be invoked from root of the project after mvn package

set -e
rm -rf bin
mkdir bin
cp target/temporal-search*.jar bin/
cp -R target/lib bin/

rsync -r --partial -h --progress bin swarm.cs.umass.edu:/work1/allan/jfoley/ts/

