FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack upgrade

COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install hlint apply-refact

# The following support files' locations are brittle. Use this RUN command to
# re-discover them when we update anything.
# RUN find /root/.stack/programs/x86_64-linux \
#   \( \
#     -name hlint.yaml -o \
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
  apt-get -y install --no-install-recommends \
    gcc \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
# BEGIN copy from RUN-find
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/llvm-targets /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/llvm-targets
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/package.conf.d /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/package.conf.d
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/include/ghcversion.h /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/include/ghcversion.h
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/llvm-passes /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/llvm-passes
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/platformConstants /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/platformConstants
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/settings /root/.stack/programs/x86_64-linux/ghc-8.8.4/lib/ghc-8.8.4/settings
# END copy from RUN-find
COPY --from=builder /root/.local/bin/hlint /usr/bin/hlint
COPY --from=builder /root/.local/bin/refactor /usr/bin/refactor
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["hlint", "--help"]
