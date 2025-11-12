FROM ubuntu:24.04
LABEL maintainer="Patrick Brisbin <pbrisbin@restyled.io>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    ca-certificates \
    curl \
    libpcre3-dev \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8

# renovate: datasource=github-releases depName=headroom package=vaclavsvejcar/headroom
ENV HEADROOM_VERSION=0.4.3.0

RUN \
  curl -L -o /usr/bin/headroom \
  https://github.com/vaclavsvejcar/headroom/releases/download/v${HEADROOM_VERSION}/headroom-${HEADROOM_VERSION}-Linux-ghc-8.10.4 && \
  chmod +x /usr/bin/headroom
RUN mkdir -p /code
WORKDIR /code

RUN mkdir -p /root/.headroom/templates

# Disables update check, since we have no network
RUN mkdir -p /root/.headroom
COPY global-config.yaml /root/.headroom/

# Add a single template we can use in tests
RUN mkdir -p /root/.headroom/templates
COPY haskell.mustache /root/.headroom/templates/

ENTRYPOINT []
CMD ["headroom", "--help"]
