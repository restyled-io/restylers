FROM maven:3.8.2-jdk-11
LABEL maintainer=pbrisbin@gmail.com
ENV FORMATTER_VERSION 2.13.0
RUN mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=net.revelc.code.formatter:formatter-maven-plugin:$FORMATTER_VERSION
RUN mvn org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get -Dartifact=org.apache.maven.plugins:maven-clean-plugin:2.5
RUN mkdir -p /code
WORKDIR /code
COPY files /
