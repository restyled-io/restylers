FROM python:3.8-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN \
  apk add --no-cache --virtual .build-deps \
    gcc=9.3.0-r2 \
    musl-dev=1.1.24-r9 && \
  pip install black==20.8b1 && \
  apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["black", "--help"]
