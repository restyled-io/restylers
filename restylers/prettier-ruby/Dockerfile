FROM node:lts-alpine3.11 as builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8

# hadolint ignore=DL3018,DL3019
RUN \
  apk --update add bash g++ linux-headers make && \
  cd /tmp && \
  wget -O ruby-install-0.7.0.tar.gz https://github.com/postmodern/ruby-install/archive/v0.7.0.tar.gz && \
  tar -xzvf ruby-install-0.7.0.tar.gz && \
  cd ruby-install-0.7.0/ && \
  make install && \
  ruby-install --system ruby-2.5

FROM node:lts-alpine3.11
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
COPY --from=builder /usr/local/bin/ruby /usr/local/bin/ruby
COPY --from=builder /usr/local/lib/ruby /usr/local/lib/ruby
ENV PRETTIER_VERSION_JS 1.18.2
ENV PRETTIER_VERSION_RB 0.15.0
RUN npm install -g \
  "prettier@$PRETTIER_VERSION_JS" \
  "@prettier/plugin-ruby@$PRETTIER_VERSION_RB"

RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["prettier", "--help"]
