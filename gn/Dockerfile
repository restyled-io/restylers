FROM ubuntu:22.04 as builder
LABEL maintainer="Michael Spang <spang@google.com>"
ENV LANG en_US.UTF-8
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      wget \
      unzip && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*
RUN wget -q -O gn.zip https://chrome-infra-packages.appspot.com/dl/gn/gn/linux-amd64/+/Z6cz3BKAP2GgVlujo5D6CFdrLE3B1hXFwVwraB6QBnUC && \
    unzip -d /usr/local/bin gn.zip gn && \
    rm gn.zip
FROM ubuntu:22.04
COPY --from=builder /usr/local/bin/gn /usr/local/bin/gn
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["gn", "--help"]
