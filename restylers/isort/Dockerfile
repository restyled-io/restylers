FROM python:3.9-alpine
LABEL maintainer=sergio@garciparedes.me
ENV LANG C.UTF-8
RUN \
  apk add --no-cache --virtual .build-deps \
    gcc \
    musl-dev && \
  pip install --no-cache-dir isort==5.6.4 && \
  apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["isort", "--help"]
