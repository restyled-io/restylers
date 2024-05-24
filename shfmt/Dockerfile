FROM golang:alpine as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN go install mvdan.cc/sh/v3/cmd/shfmt@v3.4.3
RUN find / -name 'shfmt'

FROM alpine:3.20
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
COPY --from=builder /go/bin/shfmt /usr/bin/shfmt
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["shfmt", "--help"]
