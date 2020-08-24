FROM debian:jessie AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    clang=1:3.5-25 \
    g++=4:4.9.2-2 \
    locales=2.19-18+deb8u10 \
    make=4.0-8.1 && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  locale-gen
ENV LANG en_US.UTF-8
ADD astyle_3.1_linux.tar.gz /tmp/
WORKDIR /tmp/astyle/build/clang
RUN make

FROM debian:jessie
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND noninteractive
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    locales=2.19-18+deb8u10 && \
  apt-get clean && \
  rm -rf /var/lib/apt/lists/*
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
  locale-gen
ENV LANG en_US.UTF-8
COPY --from=builder /tmp/astyle/build/clang/bin/astyle /usr/bin/astyle
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["astyle", "--help"]
