FROM restyled/stack-build-minimal:22.04 as builder
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
#   -printf "COPY --from=builder %p %p\n"; exit 1

FROM ubuntu:22.04
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
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/llvm-passes /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/llvm-passes
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/llvm-targets /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/llvm-targets
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/settings /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/settings
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/package.conf.d /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/package.conf.d
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/x86_64-linux-ghc-9.4.5/rts-1.0.2/include/ghcversion.h /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.5/lib/ghc-9.4.5/lib/x86_64-linux-ghc-9.4.5/rts-1.0.2/include/ghcversion.h
# END copy from RUN-find
COPY --from=builder /root/.local/bin/hlint /usr/bin/hlint
COPY --from=builder /root/.local/bin/refactor /usr/bin/refactor
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["hlint", "--help"]
