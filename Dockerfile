FROM haskell:7.10.3

WORKDIR /app

# dependency of hmatrix
RUN apt-get update && apt-get install -y libblas-dev libatlas-dev liblapack-dev

RUN cabal update

COPY . /app

# create .cabal-sandbox on runtime
CMD cabal sandbox init && \
    cabal install --only-dependencies -j4 && \
    cabal install hlint && \
    cabal configure --enable-tests && \
    cabal build && \
    cabal test --show-details=always && \
    /bin/bash -c "cabal exec hlint -- $(cat .hlint)"
