FROM fpco/stack-build-small:lts-16.12 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /src
WORKDIR /src

COPY stack.yaml package.yaml /src/
RUN stack install --dependencies-only
COPY src /src/src
COPY app /src/app
RUN stack install

# Docker client
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates=20190110~18.04.1 \
    locales=2.27-3ubuntu1.2 \
    netbase=5.4 && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*

COPY --from=builder /root/.local/bin/restylers /bin/restylers
COPY --from=builder /usr/local/bin/docker /usr/local/bin/docker
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["restylers", "--help"]
