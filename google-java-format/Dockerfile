FROM amazoncorretto:25-alpine3.22-jdk
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG=en_US.UTF-8
RUN apk update && apk add --no-cache bash curl

# renovate: datasource=maven depName=google-java-format packageName=com.google.googlejavaformat
ENV GOOGLE_JAVA_FORMAT_VERSION=1.32.0

RUN curl -sSf -L -o /opt/google-java-format.jar \
  "https://repo.maven.apache.org/maven2/com/google/googlejavaformat/google-java-format/$GOOGLE_JAVA_FORMAT_VERSION/google-java-format-$GOOGLE_JAVA_FORMAT_VERSION-all-deps.jar"
COPY google-java-format /usr/local/bin/google-java-format
RUN mkdir -p /code
WORKDIR /code
CMD ["google-java-format", "--help"]
