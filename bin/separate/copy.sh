#!/bin/bash

bin_dir=$(cd $(dirname $0) && pwd)
parent_dir=$(cd $bin_dir/../.. && pwd)
root_dir=$(cd $parent_dir/.. && pwd)
public_dir=$(cd $root_dir/app/public && pwd)
dist_dir=$(cd $parent_dir/app/ && pwd)

echo $public_dir
rm -rf $public_dir/rooper
cp -r  $dist_dir/public $public_dir/rooper
