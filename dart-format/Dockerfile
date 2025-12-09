FROM dart:3.10.4
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
RUN mkdir -p /code
WORKDIR /code
RUN dart --disable-analytics
CMD ["dart", "format", "--help"]
