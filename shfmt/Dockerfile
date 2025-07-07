FROM golang:alpine as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV SHFMT_VERSION=3.4.3
RUN go install mvdan.cc/sh/v3/cmd/shfmt@v$SHFMT_VERSION
RUN find / -name 'shfmt'

FROM alpine:3.22
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
COPY --from=builder /go/bin/shfmt /usr/bin/shfmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shfmt", "--help"]
