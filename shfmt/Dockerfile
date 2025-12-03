FROM golang:alpine as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8

# renovate: datasource=go depName=shfmt packageName=mvdan.cc/sh/v3
ENV SHFMT_VERSION=3.12.0

RUN go install mvdan.cc/sh/v3/cmd/shfmt@v$SHFMT_VERSION
RUN find / -name 'shfmt'

FROM alpine:3.23
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
COPY --from=builder /go/bin/shfmt /usr/bin/shfmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shfmt", "--help"]
