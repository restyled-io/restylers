FROM dart:3.10.0
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN mkdir -p /code
WORKDIR /code
RUN dart --disable-analytics
CMD ["dart", "format", "--help"]
