FROM restyled/stack-build-minimal:22.04 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
RUN stack upgrade
COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install hindent

FROM ubuntu:22.04
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
