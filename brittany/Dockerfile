FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack upgrade

# Dumb cache-buster. Bump this to force a build
ENV _DOCKERFILE_VERSION 0

ENV BRITTANY_VERSION 0.13.1.0
RUN \
  git clone https://github.com/lspitzner/brittany.git && cd brittany && \
  git reset --hard "$BRITTANY_VERSION" && \
  stack setup && \
  stack install

# The following support files' locations are brittle. Use this RUN command to
# re-discover them when we update anything.
# RUN find /root/.stack/programs/x86_64-linux \
#   \( \
#     -name settings -o \
#     -name platformConstants -o \
#     -name llvm-targets -o \
#     -name llvm-passes -o \
#     -name package.conf.d -o \
#     -name ghcversion.h \
#   \) \
#   -printf "COPY --from=builder %p %p\n"
# RUN exit 1

FROM ubuntu:20.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y --no-install-recommends install \
    gcc \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
# BEGIN copy from RUN-find
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/llvm-targets /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/llvm-targets
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/package.conf.d /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/package.conf.d
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/include/ghcversion.h /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/include/ghcversion.h
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/llvm-passes /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/llvm-passes
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/platformConstants /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/platformConstants
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/settings /root/.stack/programs/x86_64-linux/ghc-8.10.2/lib/ghc-8.10.2/settings
# END copy from RUN-find
COPY --from=builder /root/.local/bin/brittany /usr/bin/brittany
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["brittany", "--help"]
