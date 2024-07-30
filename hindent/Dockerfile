FROM fpco/stack-build-small:lts-22.31 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN stack update # cache cabal files update
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install hindent

FROM ubuntu:24.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN \
  apt-get update && \
  apt-get -y install --no-install-recommends locales && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/hindent /usr/bin/hindent
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["hindent", "--help"]
