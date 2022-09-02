FROM php:8.1.10-cli-alpine3.15
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV COMPOSER_INSTALL=https://raw.githubusercontent.com/composer/getcomposer.org/76a7060ccb93902cd7576b67264ad91c8a2700e2/web/installer
RUN mkdir -p /app
WORKDIR /app
# hadolint ignore=DL3008,DL4006
RUN curl -sL "$COMPOSER_INSTALL" | php -- --quiet
COPY composer.json .
RUN php composer.phar install
ENV PATH=/app/vendor/bin:$PATH
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["php-cs-fixer", "fix", "--help"]
