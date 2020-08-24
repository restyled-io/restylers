FROM dlanguage/dmd:2.080.0 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
ENV DFMT_VERSION v0.8.2
ENV LANG en_US.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    build-essential=12.1ubuntu2 \
    git=1:2.7.4-0ubuntu1 && \
  rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/dlang-community/dfmt /dfmt
WORKDIR /dfmt
RUN git reset --hard "$DFMT_VERSION"
RUN git submodule update --init
RUN make

FROM ubuntu:16.10
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
COPY --from=builder /dfmt/bin/dfmt /usr/bin/dfmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["dfmt", "--help"]
