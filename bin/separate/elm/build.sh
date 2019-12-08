#!/bin/bash

bin_dir=$(cd $(dirname $0) && pwd)
parent_dir=$bin_dir/../../..
docker_dir=$parent_dir/docker
dir_docker=$docker_dir
name=${1:-rooper}
inputFile="/app/src/assets/js/elm/Main.elm"
outputFile="/app/separate/pre-pre-dist/assets/js/elm/Main.js"
minFile="/app/separate/pre-pre-dist/assets/js/elm/Main.js"

# docker-composeの起動。 
cd $dir_docker  && docker-compose run $name /bin/bash -c "yarn run elm make $inputFile --output=$outputFile --optimize"
