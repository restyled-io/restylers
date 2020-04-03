FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
label maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack upgrade

ENV STACK_RESOLVER lts-14.3
ENV ORMOLU_VERSION 0.0.1.0

RUN \
  git clone https://github.com/tweag/ormolu.git && cd ormolu && \
  git reset --hard "$ORMOLU_VERSION" && \
  echo "resolver: $STACK_RESOLVER" > stack.yaml && \
  stack setup && \
  stack install

FROM ubuntu:18.10
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

# Brittany runtime files
COPY --from=builder /root/.local/bin/ormolu /usr/bin/ormolu
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/settings \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/settings
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/llvm-targets \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/llvm-targets
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/llvm-passes \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/llvm-passes
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/platformConstants \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/platformConstants
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/package.conf.d \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/package.conf.d

# Needed for CPP
#COPY --from=builder \
#  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/include/ghcversion.h \
#  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/include/ghcversion.h

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["ormolu", "--help"]
