FROM csanchez/maven:4.0-eclipse-temurin-17-maven-4
LABEL maintainer=pbrisbin@gmail.com

# renovate: datasource=maven depName=jdt packageName=net.revelc.code.formatter:formatter-maven-plugin
ENV FORMATTER_VERSION=2.29.0

RUN mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=net.revelc.code.formatter:formatter-maven-plugin:$FORMATTER_VERSION
RUN mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=org.apache.maven.plugins:maven-clean-plugin:3.2.0
RUN mkdir -p /code
WORKDIR /code
COPY files/bin/formatter /usr/local/bin/formatter
