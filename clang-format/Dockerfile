FROM ubuntu:20.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN \
  apt-get update -y && \
  apt-get install -y --no-install-recommends \
    clang-format=1:10.0-50~exp1 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["clang-format", "-help"]
