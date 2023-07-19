FROM python:3.11-alpine
LABEL maintainer=asottile@umich.edu
ENV LANG C.UTF-8
RUN apk add --no-cache bash git

RUN mkdir /app
WORKDIR /app
COPY requirements.txt .
RUN pip install --no-cache-dir --requirement requirements.txt

COPY config.yaml /app/.pre-commit-config.yaml
RUN git init && pre-commit install-hooks && rm -rf .git

WORKDIR /code
COPY validate /usr/local/bin/validate-pre-commit
COPY entrypoint.sh /
ENTRYPOINT ["/entrypoint.sh"]
CMD ["black", "--help"]
