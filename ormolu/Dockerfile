FROM restyled/stack-build-minimal:24.04 as builder
label maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack upgrade

COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install ormolu

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

FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8
# BEGIN copy from RUN-find
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/llvm-passes /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/llvm-passes
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/llvm-targets /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/llvm-targets
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/settings /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/settings
COPY --from=builder /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/package.conf.d /root/.stack/programs/x86_64-linux/ghc-tinfo6-9.4.4/lib/ghc-9.4.4/lib/package.conf.d
# END copy from RUN-find
COPY --from=builder /root/.local/bin/ormolu /usr/bin/ormolu
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["ormolu", "--help"]
