FROM alpine:3.23
LABEL maintainer="Patrick Brisbin <pbrisbin@gmail.com>"

# renovate: datasource=github-tags depName=pg_format packageName=darold/pgFormatter
ENV PG_FORMAT_VERSION=5.9
ENV PG_FORMAT_ARCHIVE=https://github.com/darold/pgFormatter/archive/v$PG_FORMAT_VERSION.tar.gz

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
