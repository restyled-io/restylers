FROM debian:bookworm AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    clang \
    g++ \
    locales \
    make && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  locale-gen
ENV LANG en_US.UTF-8
ADD astyle-3.6.2.tar.bz2 /tmp/
WORKDIR /tmp/astyle-3.6.2/build/clang
RUN make

FROM debian:bookworm
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    locales && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  locale-gen
ENV LANG en_US.UTF-8
COPY --from=builder /tmp/astyle-3.6.2/build/clang/bin/astyle /usr/bin/astyle
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["astyle", "--help"]
