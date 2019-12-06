#!/bin/bash

bin_dir=$(cd $(dirname $0) && pwd)
parent_dir=$(cd $bin_dir/../.. && pwd)
dist_dir=$(cd $parent_dir/separate/dist && pwd)
pre_dist_dir=$(cd $parent_dir/separate/pre-dist && pwd)
app_dir=$(cd $parent_dir/app && pwd)

rm -rf $app_dir/public

# rooperディレクトリとhtmlファイルをコピー
cp -r $dist_dir $app_dir/public

mkdir -p $app_dir/public/assets

# cssファイルをコピー
cp -r $pre_dist_dir/assets/css $app_dir/public/assets/css