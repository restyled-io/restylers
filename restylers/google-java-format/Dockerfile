FROM java:openjdk-14-jre
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
ENV GOOGLE_JAVA_FORMAT_JAR \
  https://github.com/google/google-java-format/releases/download/google-java-format-1.9/google-java-format-1.9-all-deps.jar
RUN curl \
  --location \
  --output /google-java-format.jar "$GOOGLE_JAVA_FORMAT_JAR"
RUN printf "%s\n" "#!/bin/sh" "exec java -jar /google-java-format.jar \"\$@\"" \
  > /usr/bin/google-java-format && \
  chmod +x /usr/bin/google-java-format
RUN mkdir -p /code
WORKDIR /code
CMD ["google-java-format", "--help"]
