[![Build Status](https://travis-ci.org/hi-ogawa/haskell_playground.png?branch=master)](https://travis-ci.org/hi-ogawa/haskell_playground)

[![Build Status](http://ec2-52-193-100-14.ap-northeast-1.compute.amazonaws.com/buildStatus/icon?job=haskell_playground)](http://ec2-52-193-100-14.ap-northeast-1.compute.amazonaws.com/job/haskell_playground/)

__Execute program__


```
# my environment
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.10.2

# as binary
$ cabal build
$ ./dist/build/main/main 10065 < ./resources/UVA10065.input

# as script
$ runHaskell src/Uva/P10065.hs < ./resources/UVA10065.input

# tips for checking diff using bash syntax: http://askubuntu.com/questions/229447/how-do-i-diff-the-output-of-two-commands
$ diff <(runHaskell src/Uva/P10065.hs < ./resources/UVA10065.input) <(cat ./resources/UVA10065.output)

# run tests (see .travis.yml)
$ cabal configure --enable-tests && cabal build && cabal test --show-details=always
```

__test in docker__

```
$ docker build -t hiogawa/haskell_playground_test .
$ docker run --rm -v $PWD/.cabal-sandbox.docker:/app/.cabal-sandbox hiogawa/haskell_playground_test
```

__References__

- input output is taken from uDebug: https://www.udebug.com/
