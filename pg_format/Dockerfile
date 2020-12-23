FROM alpine:3.12
LABEL maintainer="Patrick Brisbin <pbrisbin@gmail.com>"

ENV PG_FORMAT_VERSION 3.3
ENV PG_FORMAT_RELEASE \
  https://github.com/darold/pgFormatter/archive/v$PG_FORMAT_VERSION.tar.gz

RUN apk add --no-cache \
  curl \
  perl \
  make

RUN \
  cd /tmp && \
  curl -L -o pgFormatter.tar.gz "$PG_FORMAT_RELEASE" && \
  tar xzf pgFormatter.tar.gz && \
  cd pgFormatter-$PG_FORMAT_VERSION && \
  perl Makefile.PL && \
  make && \
  make install && \
  cd /tmp && \
  rm -rf ./pgFormatter.tar.gz ./pgFormatter-$PG_FORMAT_VERSION

RUN mkdir -p /code
WORKDIR /code
COPY files /
CMD ["pg_format", "--help"]
