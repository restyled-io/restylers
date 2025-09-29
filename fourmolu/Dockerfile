FROM ubuntu:24.04
LABEL maintainer="Tristan de Cacqueray <tdecacqu@redhat.com>"
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
ENV FOURMOLU_VERSION=0.19.0.1
RUN \
  curl -L -o /usr/bin/fourmolu \
  https://github.com/fourmolu/fourmolu/releases/download/v${FOURMOLU_VERSION}/fourmolu-${FOURMOLU_VERSION}-linux-x86_64 && \
  chmod +x /usr/bin/fourmolu
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["fourmolu", "--help"]
