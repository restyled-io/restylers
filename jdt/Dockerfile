FROM maven:3.8.6-jdk-11
LABEL maintainer=pbrisbin@gmail.com

# renovate: datasource=maven depName=jdt packageName=net.revelc.code.formatter:formatter-maven-plugin
ENV FORMATTER_VERSION=2.29.0

RUN mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=net.revelc.code.formatter:formatter-maven-plugin:$FORMATTER_VERSION
RUN mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=org.apache.maven.plugins:maven-clean-plugin:2.5
RUN mkdir -p /code
WORKDIR /code
COPY files /
