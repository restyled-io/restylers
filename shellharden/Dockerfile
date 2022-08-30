FROM rust:alpine3.10
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8

# https://github.com/rust-lang/cargo/issues/10303#issuecomment-1015007926
RUN \
  apk add --no-cache --virtual .build-deps git && \
  CARGO_NET_GIT_FETCH_WITH_CLI=true cargo install shellharden --vers 4.1.1 && \
  apk del .build-deps

WORKDIR /code
ENTRYPOINT []
CMD ["shellharden", "--help"]
