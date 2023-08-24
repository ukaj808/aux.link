FROM haskell:9.2.7
RUN cabal update && cabal build
ENTRYPOINT ["pandoc"]
