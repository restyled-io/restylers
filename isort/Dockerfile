FROM python:3.12-alpine
LABEL maintainer=sergio@garciparedes.me
ENV LANG C.UTF-8
COPY requirements.txt .
RUN \
  apk add --no-cache --virtual .build-deps \
    gcc \
    musl-dev && \
  pip install --no-cache-dir --requirement requirements.txt && \
  apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["isort", "--help"]
