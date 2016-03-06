[![Build Status](https://travis-ci.org/hi-ogawa/haskell_playground.png?branch=master)](https://travis-ci.org/hi-ogawa/haskell_playground)

__Execute program__


```
# as binary
$ cabal build
$ ./dist/build/main/main 10065 < ./resources/UVA10065.input

# as script
$ runHaskell src/Uva/P10065.hs < ./resources/UVA10065.input

# tips for checking diff using bash syntax: http://askubuntu.com/questions/229447/how-do-i-diff-the-output-of-two-commands
$ diff <(runHaskell src/Uva/P10065.hs < ./resources/UVA10065.input) <(cat ./resources/UVA10065.output)
```

__References__

- input output is taken from uDebug: https://www.udebug.com/
