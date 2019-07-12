#!/bin/bash

if [ -z "$STACK_YAML" ]; then
    ghc --version
    cabal --version
    cabal new-update
    cabal new-build --enable-tests --enable-benchmarks
else
    # install stack
    curl -sSL https://get.haskellstack.org/ | sh

    # build project with stack
    stack --version
    stack build --system-ghc --test --bench --no-run-tests --no-run-benchmarks
fi
