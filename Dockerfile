FROM haskell:9.2.4-buster

WORKDIR /work

ADD dist-newstyle/sdist/smopeck-0.1.0.0.tar.gz /work

RUN cd smopeck-0.1.0.0 && cabal v2-update && \
    cabal v2-configure --enable-executable-static && \
    cabal v2-install -j4

FROM debian:buster-slim
ARG TARGETARCH
COPY --from=0 /root/.cabal/store/ghc-9.2.4/ /root/.cabal/store/ghc-9.2.4/
COPY --from=0 /root/.cabal/bin /root/.cabal/bin
RUN apt-get update \
    && apt-get install -y libgmp10 \
    && apt-get -y clean \
    && rm -rf /var/lib/apt/lists/*

ENTRYPOINT ["/root/.cabal/bin/smopeck"]
