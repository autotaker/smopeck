FROM haskell:8.8.3-stretch

WORKDIR /work

ADD dist-newstyle/sdist/smopeck-0.1.0.0.tar.gz /work

RUN cd smopeck-0.1.0.0 && cabal v2-update && \
    cabal v2-configure --enable-executable-static && \
    cabal v2-install -j4

FROM debian:stretch-slim

COPY --from=0 /root/.cabal/store/ghc-8.8.3/ /root/.cabal/store/ghc-8.8.3/
COPY --from=0 /root/.cabal/bin /root/.cabal/bin
COPY --from=0 /usr/lib/x86_64-linux-gnu/libgmp.so.10 /usr/lib/x86_64-linux-gnu/libgmp.so.10

ENTRYPOINT ["/root/.cabal/bin/smopeck"]
