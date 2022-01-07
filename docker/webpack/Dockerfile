FROM node:14.11.0
# FROM node:12.13.1

# コンテナ上の作業ディレクトリ作成
WORKDIR /app

# https://teppeis.hatenablog.com/entry/2015/12/configure-npm-init
COPY .npmrc /app/.npmrc
COPY package.json /app/package.json
RUN npm i

