FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@restyled.io>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get --no-install-recommends -y install \
    ca-certificates \
    curl \
    locales \
    patch \
    xz-utils && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8

# renovate: datasource=github-releases depName=shellcheck packageName=koalaman/shellcheck
ENV SHELLCHECK_VERSION=0.11.0
ENV SHELLCHECK_ARCHIVE=https://github.com/koalaman/shellcheck/releases/download/v${SHELLCHECK_VERSION}/shellcheck-v${SHELLCHECK_VERSION}.linux.x86_64.tar.xz
RUN \
  curl -L "$SHELLCHECK_ARCHIVE" | tar xJf - && \
  mv "shellcheck-v${SHELLCHECK_VERSION}/shellcheck" /usr/bin/shellcheck && \
  shellcheck --version
COPY shellcheck-fix /usr/bin/shellcheck-fix
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shellcheck-fix", "--help"]
