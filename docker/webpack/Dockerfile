FROM node:14.11.0
# FROM node:12.13.1

# コンテナ上の作業ディレクトリ作成
WORKDIR /app

# https://teppeis.hatenablog.com/entry/2015/12/configure-npm-init
COPY .npmrc /app/.npmrc
RUN npm init -y

## elm
RUN yarn add --dev elm
RUN yarn add --dev elm-format
RUN yarn add --dev elm-minify
RUN yarn add --dev elm-webpack-loader
# > elm-hot-webpack-loader@1.1.2" has unmet peer dependency "elm-webpack-loader@^6.0.0".

RUN yarn add --dev elm-test
RUN yarn add --dev elm-verify-examples

## webpackインストール
RUN yarn add --dev webpack
RUN yarn add --dev webpack-cli
RUN yarn add --dev webpack-merge
RUN yarn add --dev webpack-dev-server

## plugin
RUN yarn add --dev mini-css-extract-plugin
RUN yarn add --dev html-minifier-webpack-plugin
RUN yarn add --dev clean-webpack-plugin
RUN yarn add --dev copy-webpack-plugin
RUN yarn add --dev html-webpack-plugin

### loaders
RUN yarn add --dev babel-loader
RUN yarn add --dev file-loader
RUN yarn add --dev style-loader
RUN yarn add --dev css-loader
RUN yarn add --dev html-loader
RUN yarn add --dev elm-webpack-loader
RUN yarn add --dev elm-hot-webpack-loader
RUN yarn add --dev resolve-url-loader
RUN yarn add --dev resolve-url-loader
RUN yarn add --dev sass-loader
RUN yarn add --dev url-loader

## for css
### sass
RUN yarn add --dev node-sass

### purecss
RUN yarn add purecss

## 以下、追加

### lint
RUN yarn add --dev eslint \
  eslint-plugin-import \
  eslint-config-airbnb-base \
  eslint-plugin-jasmine

### esdoc
RUN yarn add --dev esdoc esdoc-standard-plugin

## for ts
RUN yarn add --dev typescript
RUN yarn add --dev tslint tslint-config-airbnb
RUN yarn add --dev typedoc

### postcss
RUN yarn add --dev postcss postcss-cli
RUN yarn add --dev autoprefixer
RUN yarn add --dev cssnano

### lint
RUN yarn add --dev stylelint
RUN yarn add --dev stylelint-scss
RUN yarn add --dev stylelint-order
RUN yarn add --dev stylelint-config-sass-guidelines

RUN yarn add --dev ts-loader
## file copy
RUN yarn add --dev cpx
RUN yarn add --dev postcss-loader
RUN yarn add --dev @types/firebase

RUN yarn add --dev  source-map
# Cannot find module 'source-map'のエラーが出るため。
RUN yarn add --dev @types/source-map@0.5.2


## pug
RUN yarn add --dev pug
RUN yarn add --dev pug-cli#master
RUN yarn add --dev https://github.com/pugjs/pug-cli.git
RUN yarn add --dev pug-loader

RUN sed -i -e "s/\(\"scripts\": {\)/\1\n    \"prod\": \"webpack -p\",/g" /app/package.json


### babel
RUN yarn add --dev @babel/core \
  @babel/preset-env

RUN yarn add --dev @babel/cli

# 型定義をインストール
RUN yarn add --dev firebase

# import * as vfileMessage from 'vfile-message'; 対策
# https://github.com/vmware/clarity/issues/4012
# RUN sed -i -e "s/'vfile-message'/'vfile-message\/node_modules\/vfile-message'/" /app/node_modules/@types/vfile/index.d.ts

# elm-test --watchしたときのpolling設定
RUN sed -i -e "s/\(cwd: projectRootDir,\)/\1\n    usePolling: true, /g" /app/node_modules/elm-test/lib/elm-test.js

RUN  yarn add file-saver jszip
# console.logを削除
RUN yarn add --dev terser-webpack-plugin
