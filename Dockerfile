FROM haskell

RUN cabal update && cabal install hspec