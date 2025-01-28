FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin pbrisbin@restyled.io"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
ENV CLAZY_VERSION=1.11-4ubuntu2
RUN \
  apt-get update && \
  apt-get -y --no-install-recommends install \
    clang \
    clang-tools \
    clazy="$CLAZY_VERSION" \
    cmake \
    make \
    locales \
    qt6-base-dev && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG=en_US.UTF-8
COPY clazy-standlone-apply /usr/local/bin/clazy-standalone-apply
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["clazy-standalone-apply", "--help"]
