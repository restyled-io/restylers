FROM ruby:3.0.3-alpine3.13
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN apk add --no-cache \
  bash \
  g++ \
  linux-headers \
  make
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
