FROM golang:alpine
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
RUN \
  apk add --no-cache --virtual .build-deps git && \
  go get -u mvdan.cc/sh/cmd/shfmt && \
  apk del .build-deps git
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shfmt", "--help"]
