FROM ruby:2.7.3-alpine3.13
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN apk add --no-cache \
  bash=5.1.0-r0 \
  g++=10.2.1_pre1-r3 \
  linux-headers=5.7.8-r0 \
  make=4.3-r0
RUN mkdir -p /app
WORKDIR /app
COPY Gemfile .
RUN bundle
RUN bundle binstubs standard
ENV PATH=/app/bin:$PATH
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["standardrb", "--help"]
