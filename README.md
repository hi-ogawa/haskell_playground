[![Build Status](https://travis-ci.org/hi-ogawa/haskell_playground.png?branch=master)](https://travis-ci.org/hi-ogawa/haskell_playground)

Tools:

```
$ stack --version
Version 1.1.0 x86_64 hpack-0.13.0

$ docker -v
Docker version 1.11.1, build 5604cbe

$ docker-compose -v
docker-compose version 1.7.0, build 0d7bf73
```

Testing in Docker:

```
$ docker-compose run --rm test
```

The base container image `hiogawa/haskell:playground` comes from here: https://hub.docker.com/r/hiogawa/haskell/.

---

Some notes about my algorithm problem solutions ([programs under `src/Uva`](https://github.com/hi-ogawa/haskell_playground/tree/master/src/Uva))
are explained in [my blog with algorithm tag](http://wp.hiogawa.net/tag/algorithm/).

General things about Haskell can be also found in [my blog with haskell tag](http://wp.hiogawa.net/tag/haskell/).

List of Algorithms:

- [Prim's algorithm for minimum spanning tree](https://github.com/hi-ogawa/haskell_playground/blob/master/src/MST.hs)
- [3-opt algorithm for TSP](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Kopt.hs)
- [Fleury's algorithm for Euler tour construction](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Uva/P10054.hs)
- [A-star search for solving 15 puzzle](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Uva/P10181.hs)
- [Graham scan for convex hull](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Uva/P10065.hs)
- [Bentley-Ottmann algorithm for line segments intersections](https://github.com/hi-ogawa/haskell_playground/blob/master/src/BentleyOttmann.hs)
- [Edmonds-Karp algorithm for maximum network flow](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Uva/P10249.hs)
- [Perceptron](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Ml/Perceptron.hs)
- [Back propagation for neural network](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Ml/BackPropagation.hs)
- [Dynamic programming for finding longest path in DAG](https://github.com/hi-ogawa/haskell_playground/blob/master/src/Uva/P10131.hs)

