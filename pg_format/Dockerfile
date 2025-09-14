FROM alpine:3.22
LABEL maintainer="Patrick Brisbin <pbrisbin@gmail.com>"

ENV PG_FORMAT_TAG v5.8
ENV PG_FORMAT_ARCHIVE \
  https://github.com/darold/pgFormatter/archive/$PG_FORMAT_TAG.tar.gz

RUN apk add --no-cache \
  curl \
  perl \
  make

RUN \
  cd /tmp && \
  curl -L -o pgFormatter.tar.gz "$PG_FORMAT_ARCHIVE" && \
  tar xzf pgFormatter.tar.gz && \
  cd pgFormatter-* && \
  perl Makefile.PL && \
  make && \
  make install && \
  cd /tmp && \
  rm -rf ./pgFormatter.tar.gz ./pgFormatter-*

RUN mkdir -p /code
WORKDIR /code
CMD ["pg_format", "--help"]
