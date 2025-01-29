FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    ca-certificates \
    curl \
    locales \
    unzip && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8

ENV STYLISH_VERSION=0.14.3.0
ENV STYLISH_TAR_URL=https://github.com/haskell/stylish-haskell/releases/download/v${STYLISH_VERSION}/stylish-haskell-v${STYLISH_VERSION}-linux-x86_64.tar.gz
RUN \
  cd /tmp && \
  curl -sSf -L "$STYLISH_TAR_URL" | tar -xzf - && \
  mv ./stylish-haskell-*/stylish-haskell /usr/bin/stylish-haskell && \
  rm -rf ./stylish-haskell-*

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["stylish-haskell", "--help"]
