FROM php:7.2.11-cli-alpine3.7
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8

# Increment to force update
ENV DOCKERFILE_VERSION 1

ENV PHAR_SRC http://cs.sensiolabs.org/download/php-cs-fixer-v2.phar
RUN wget "$PHAR_SRC" -O /usr/bin/php-cs-fixer
RUN chmod +x /usr/bin/php-cs-fixer
RUN apk add --no-cache \
  bash=4.4.19-r1
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["php-cs-fixer", "fix", "--help"]
