FROM python:3.12-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN apk add --no-cache bash moreutils
COPY requirements.txt .
RUN pip install --no-cache-dir --requirement requirements.txt
WORKDIR /code
ENTRYPOINT []
CMD ["sqlformat", "--help"]
