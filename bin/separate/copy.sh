#!/bin/bash

bin_dir=$(cd $(dirname $0) && pwd)
parent_dir=$(cd $bin_dir/../.. && pwd)
dist_dir=$(cd $parent_dir/separate/dist && pwd)
app_dir=$(cd $parent_dir/app && pwd)

rm -rf $app_dir/public

cp -r  $dist_dir $app_dir/public
