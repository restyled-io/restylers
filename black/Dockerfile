FROM python:3.8-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN apk add --no-cache --virtual .build-deps gcc musl-dev
RUN pip install black==19.10b0
RUN apk del .build-deps
WORKDIR /code
ENTRYPOINT []
CMD ["black", "--help"]
