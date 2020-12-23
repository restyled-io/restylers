FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack upgrade

COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install stylish-haskell

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install --no-install-recommends locales=2.27-3ubuntu1 && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/stylish-haskell /usr/bin/stylish-haskell
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["stylish-haskell", "--help"]
