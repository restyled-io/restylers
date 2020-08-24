FROM quay.io/haskell_works/stack-build-minimal:18.04 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
RUN stack upgrade
RUN stack setup --resolver lts-13.30
RUN stack install --resolver lts-13.30 stylish-haskell-0.9.2.2

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN \
  apt-get update && \
  apt-get -y install --no-install-recommends locales=2.27-3ubuntu1 && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/stylish-haskell /usr/bin/stylish-haskell
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["stylish-haskell", "--help"]
