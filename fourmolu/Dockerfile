FROM restyled/stack-build-minimal:20.04 as builder
LABEL maintainer="Tristan de Cacqueray <tdecacqu@redhat.com>"
RUN stack upgrade

COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install fourmolu

# The following support files' locations are brittle. Use this RUN command to
# re-discover them when we update anything.
# RUN find /root/.stack/programs/x86_64-linux \
#   \( \
#     -name settings -o \
#     -name platformConstants -o \
#     -name llvm-targets -o \
#     -name llvm-passes -o \
#     -name package.conf.d \
#   \) \
#   -printf "COPY --from=builder %p %p\n"
# RUN exit 1

FROM ubuntu:22.04
LABEL maintainer="Tristan de Cacqueray <tdecacqu@redhat.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8
# BEGIN copy from RUN-find
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/llvm-passes /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/llvm-passes
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/settings /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/settings
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/llvm-targets /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/llvm-targets
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/package.conf.d /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/package.conf.d
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/platformConstants /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.0.2/lib/ghc-9.0.2/platformConstants
# END copy from RUN-find
COPY --from=builder /root/.local/bin/fourmolu /usr/bin/fourmolu
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["fourmolu", "--help"]
