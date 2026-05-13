FROM alpine:3.23.4
LABEL maintainer="Patrick Brisbin <pbrisbin@gmail.com>"
RUN apk add --no-cache bash curl

# renovate: datasource=github-releases depName=shellharden packageName=anordal/shellharden
ENV SHELLHARDEN_VERSION=4.3.1

ENV SHELLHARDEN_SRC_URL=https://github.com/anordal/shellharden/releases/download/v${SHELLHARDEN_VERSION}/shellharden-x86_64-unknown-linux-musl.tar.gz

RUN \
  cd /tmp && \
  curl -Ssf -L "$SHELLHARDEN_SRC_URL" | tar xzf - && \
  mv ./shellharden /usr/bin/shellharden

WORKDIR /code
ENTRYPOINT []
CMD ["shellharden", "--help"]
