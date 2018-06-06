FROM alpine:3.6
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
RUN apk add --update bash curl unzip

ENV TERRAFORM_VERSION 0.11.7
ENV TERRAFORM_ARCHIVE terraform_${TERRAFORM_VERSION}_linux_amd64.zip
ENV TERRAFORM_URL \
  https://releases.hashicorp.com/terraform/$TERRAFORM_VERSION/$TERRAFORM_ARCHIVE

RUN \
  cd /tmp && \
  curl -O "$TERRAFORM_URL" && \
  unzip "$TERRAFORM_ARCHIVE" && \
  cp -v ./terraform /usr/bin

RUN mkdir -p /code
WORKDIR /code
COPY files /
CMD ["terraform-fmt-multi", "--help"]
