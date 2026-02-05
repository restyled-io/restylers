FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
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

# renovate: datasource=hackage depName=hindent versioning=semver
ENV HINDENT_VERSION=6.2.1

RUN \
  curl -L -o /usr/bin/hindent \
  https://github.com/mihaimaruseac/hindent/releases/download/v${HINDENT_VERSION}/hindent && \
  chmod +x /usr/bin/hindent
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["hindent", "--help"]
