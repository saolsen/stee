#!/usr/bin/env bash

curl https://sh.rustup.rs -sSf | sh -s -- -y
rustup install nightly
rustup default nightly
source $HOME/.cargo/env
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
cd website
npm install
cd ../stee-js
wasm-pack build
cd pkg
npm link
cd ../../website
npm link stee-js
npm run build