#!/bin/sh
set -ex
cargo build --workspace

cd macros
cargo publish
cd ..
cargo publish
