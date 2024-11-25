#!/bin/bash

rm -rf profraw/*.*

export RUSTFLAGS="-Cinstrument-coverage"
cargo build

export LLVM_PROFILE_FILE="profraw/%p-%m.profraw"
cargo test

grcov profraw/ -s . --binary-path ./target/debug/ -t html --branch --ignore-not-existing -o ./coverage/