FROM python:3.13-alpine
LABEL maintainer=asottile@umich.edu
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
CMD ["black", "--help"]
