#!/bin/bash

docker build -t hiogawa/haskell_playground_test .
docker run --rm -v $PWD/.cabal-sandbox.docker:/app/.cabal-sandbox hiogawa/haskell_playground_test
