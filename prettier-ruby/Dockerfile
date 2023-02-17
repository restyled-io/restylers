FROM ruby:3.2.1
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
# hadolint ignore=DL3008,DL4006
RUN curl -sL https://deb.nodesource.com/setup_15.x | bash - && \
  apt-get install -y --no-install-recommends nodejs
RUN mkdir -p /app
WORKDIR /app
COPY Gemfile .
RUN bundle
RUN bundle binstubs prettier
ENV PATH=/app/bin:$PATH
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["rbprettier", "--help"]
