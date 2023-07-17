FROM perl:5 AS builder
RUN mkdir -p build
ENV PERLTIDY_TAG 20230701
ENV PERLTIDY_SRC https://github.com/perltidy/perltidy/archive/refs/tags/${PERLTIDY_TAG}.tar.gz
RUN \
  bash -c 'set -o pipefail; curl -L "$PERLTIDY_SRC" | tar xzf - -C /tmp' && \
  cd /tmp/perltidy* && \
  perl Makefile.PL && \
  make install

FROM perl:slim
LABEL maintainer="Pat Brisbin pbrisbin@restyled.io"
COPY --from=builder /usr/local /usr/local
WORKDIR /code
ENTRYPOINT []
CMD ["perltidy", "--help"]
