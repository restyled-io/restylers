FROM ruby:alpine3.10
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN apk add --no-cache \
  bash=5.0.0-r0 \
  g++=8.3.0-r0 \
  linux-headers=4.19.36-r0 \
  make=4.2.1-r2
RUN gem install \
  rubocop:0.72.0 \
  rubocop-performance:1.5.2 \
  rubocop-rails:2.4.0
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["rubocop", "--help"]
