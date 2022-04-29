FROM openjdk:18.0.1
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN mkdir -p /app
WORKDIR /app
COPY build.gradle install ./
RUN ./install build.gradle
ENV PATH=/app/bin:$PATH
RUN mkdir -p /code
WORKDIR /code
CMD ["google-java-format", "--help"]
