FROM restyled/stack-build-minimal:24.04 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack upgrade

COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install cabal-fmt

FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install --no-install-recommends locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8
COPY --from=builder /root/.local/bin/cabal-fmt /usr/bin/cabal-fmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["cabal-fmt", "--help"]
