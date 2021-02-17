FROM alpine:3.13.2
LABEL maintainer="Patrick Brisbin <pbrisbin@gmail.com>"
RUN \
  apk add --no-cache \
    bash=5.0.17-r0 \
    curl=7.69.1-r3
ENV JQ_VERSION=1.6
RUN curl -L -o /usr/bin/jq \
  https://github.com/stedolan/jq/releases/download/jq-$JQ_VERSION/jq-linux64
RUN chmod +x /usr/bin/jq
RUN mkdir -p /code
WORKDIR /code
COPY files /
CMD ["jq-write", "--help"]
