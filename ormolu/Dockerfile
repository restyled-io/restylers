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

ENV ORMOLU_VERSION=0.5.3.0
ENV ORMOLU_ZIP_URL=https://github.com/tweag/ormolu/releases/download/${ORMOLU_VERSION}/ormolu-Linux.zip
RUN \
  cd /tmp && \
  curl -sSf -L -o ormolu.zip "$ORMOLU_ZIP_URL" && \
  unzip ./ormolu.zip && \
  chmod +x ./ormolu && \
  mv ./ormolu /usr/bin/ormolu && \
  rm -rf ./ormolu.zip

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["ormolu", "--help"]
