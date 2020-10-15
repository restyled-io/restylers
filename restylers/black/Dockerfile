FROM python:3.9-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN \
  apk add --no-cache --virtual .build-deps \
    gcc \
    musl-dev && \
  pip install black==20.8b1 && \
  apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["black", "--help"]
