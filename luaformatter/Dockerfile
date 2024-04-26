# Based on https://github.com/ekisu/docker-luaformatter (MIT Licensed)
FROM ubuntu:24.04 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@restyled.io>"
ENV DEBIAN_FRONTEND noninteractive

RUN sed --in-place --regexp-extended "s/(\/\/)(archive\.ubuntu)/\1br.\2/" /etc/apt/sources.list
RUN apt-get update && apt-get install -y --no-install-recommends \
  bash \
  unzip \
  make \
  build-essential \
  dnsutils \
  libyaml-dev \
  libpcre3-dev \
  libssl-dev \
  vim \
  luarocks \
  git \
  ca-certificates \
  cmake && \
  apt-get clean

RUN luarocks install --server=https://luarocks.org/dev luaformatter

FROM ubuntu:24.04
COPY --from=builder /usr/local/bin/lua-format /usr/local/bin/lua-format
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD [ "lua-format", "--help" ]
