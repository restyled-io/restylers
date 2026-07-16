FROM ubuntu:26.04
LABEL maintainer="Chris Martin <ch.martin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    ca-certificates \
    curl \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8

# renovate: datasource=hackage depName=cabal-gild
ENV CABAL_GILD_VERSION=1.8.4.1

RUN \
  curl -L -o /tmp/cabal-gild.tar.gz \
  https://github.com/tfausak/cabal-gild/releases/download/${CABAL_GILD_VERSION}/cabal-gild-${CABAL_GILD_VERSION}-linux-x64.tar.gz && \
  tar -xzf /tmp/cabal-gild.tar.gz -C /usr/bin && \
  chmod +x /usr/bin/cabal-gild && \
  rm /tmp/cabal-gild.tar.gz
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["cabal-gild", "--help"]
