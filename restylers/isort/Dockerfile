FROM python:3.8-alpine
LABEL maintainer=sergio@garciparedes.me
ENV LANG C.UTF-8
RUN \
  apk add --no-cache --virtual .build-deps \
    gcc \
    musl-dev && \
  pip install isort==5.4.2 && \
  apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["isort", "--help"]
