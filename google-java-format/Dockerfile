FROM amazoncorretto:25-alpine3.22-jdk
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN apk update && apk add --no-cache curl
RUN mkdir -p /app
WORKDIR /app
COPY build.gradle install ./
RUN ./install build.gradle
ENV PATH=/app/bin:$PATH
RUN mkdir -p /code
WORKDIR /code
CMD ["google-java-format", "--help"]
