FROM ubuntu:19.10
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN \
  apt-get update -y && \
  apt-get install -y clang-format
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["clang-format", "-help"]
