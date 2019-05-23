FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
RUN stack upgrade

# Determines HLint version. lts-13.2 -> hlint-2.1.11 (apply-refact-0.6.0.0)
ENV STACKAGE_RESOLVER lts-13.2
RUN stack --resolver "$STACKAGE_RESOLVER" install hlint apply-refact

FROM ubuntu:18.04
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install \
    gcc \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
COPY --from=builder /root/.local/bin/hlint /usr/bin/hlint
COPY --from=builder \
  /root/.stack/snapshots/x86_64-linux/lts-13.2/8.6.3/share/x86_64-linux-ghc-8.6.3/hlint-2.1.11/hlint.yaml \
  /root/.stack/snapshots/x86_64-linux/lts-13.2/8.6.3/share/x86_64-linux-ghc-8.6.3/hlint-2.1.11/hlint.yaml
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/settings \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/settings
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/platformConstants \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/platformConstants
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/llvm-targets \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/llvm-targets
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/llvm-passes \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/llvm-passes
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/package.conf.d \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/package.conf.d
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/include/ghcversion.h \
  /root/.stack/programs/x86_64-linux/ghc-8.6.3/lib/ghc-8.6.3/include/ghcversion.h
COPY --from=builder /root/.local/bin/refactor /usr/bin/refactor
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["hlint", "--help"]
