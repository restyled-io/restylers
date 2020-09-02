FROM python:3.8-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN \
  apk add --no-cache --virtual .build-deps \
    gcc=9.3.0-r0 \
    musl-dev=1.1.24-r2 && \
  pip install black==19.10b0 && \
  apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["black", "--help"]
