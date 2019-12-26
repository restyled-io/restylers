FROM ruby:alpine3.10
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN apk --update add bash g++ linux-headers make
RUN gem install \
  rubocop:0.72.0 \
  rubocop-performance:1.5.2 \
  rubocop-rails:2.4.0
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["rubocop", "--help"]
