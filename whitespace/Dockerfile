FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
RUN stack upgrade
RUN stack setup
RUN mkdir /src
WORKDIR /src
COPY package.yaml stack.yaml stack.yaml.lock ./
RUN stack build --test --no-run-tests --dependencies-only
COPY app ./app
COPY src ./src
COPY test ./test
RUN stack build --pedantic --test
RUN stack install

FROM ubuntu:18.10
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get -y install \
    gcc \
    locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
COPY --from=builder /root/.local/bin/whitespace /usr/bin/whitespace
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["whitespace", "--help"]
