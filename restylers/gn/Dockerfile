FROM ubuntu:20.04
LABEL maintainer="Michael Spang <spang@google.com>"
ENV LANG en_US.UTF-8
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates=20190110ubuntu1 \
      wget=1.20.3-1ubuntu1 \
      unzip=6.0-25ubuntu1 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN wget -O gn-latest.zip https://chrome-infra-packages.appspot.com/dl/gn/gn/linux-amd64/+/latest && \
    unzip -d /usr/local/bin gn-latest.zip gn && \
    rm gn-latest.zip
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["gn", "--help"]
