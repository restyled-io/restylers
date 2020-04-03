FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
RUN stack upgrade

# Dumb cache-buster. Bump this to force a build
ENV _DOCKERFILE_VERSION 0

ENV BRITTANY_VERSION 0.12.1.1
RUN \
  git clone https://github.com/lspitzner/brittany.git && cd brittany && \
  git reset --hard "$BRITTANY_VERSION" && \
  stack setup && \
  stack install

FROM ubuntu:18.10
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install \
    gcc \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

# Brittany runtime files
COPY --from=builder /root/.local/bin/brittany /usr/bin/brittany
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
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/include/ghcversion.h \
  /root/.stack/programs/x86_64-linux/ghc-8.6.5/lib/ghc-8.6.5/include/ghcversion.h

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["brittany", "--help"]
