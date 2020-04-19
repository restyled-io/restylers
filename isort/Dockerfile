FROM python:3.8-alpine
LABEL maintainer=sergio@garciparedes.me
ENV LANG C.UTF-8
RUN apk add --no-cache --virtual .build-deps gcc musl-dev
RUN pip install isort==4.3.21
RUN apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["isort", "--help"]
