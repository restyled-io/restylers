FROM dart:3.8.1
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN mkdir -p /code
WORKDIR /code
RUN dart --disable-analytics
CMD ["dart", "format", "--help"]
