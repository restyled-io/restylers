FROM maven:3.6.1-jdk-11
RUN mkdir -p /code
WORKDIR /code
RUN ["mvn", "org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get", "-Dartifact=net.revelc.code.formatter:formatter-maven-plugin:2.10.0"]
RUN ["mvn", "org.apache.maven.plugins:maven-dependency-plugin:3.1.1:get", "-Dartifact=org.apache.maven.plugins:maven-clean-plugin:2.5"]
COPY Formatter.java /script/
RUN ["javac", "/script/Formatter.java"]
ENTRYPOINT []
