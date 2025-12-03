FROM alpine:3.23.0
LABEL maintainer="Patrick Brisbin <pbrisbin@gmail.com>"
RUN apk add --no-cache bash curl

# renovate: datasource=github-releases depName=jq packageName=stedolan/jq
ENV JQ_VERSION=1.6

RUN curl -L -o /usr/bin/jq \
  https://github.com/stedolan/jq/releases/download/jq-$JQ_VERSION/jq-linux64
RUN chmod +x /usr/bin/jq
RUN mkdir -p /code
WORKDIR /code
COPY files /
CMD ["jq-write", "--help"]
