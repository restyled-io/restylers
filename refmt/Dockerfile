FROM node:20.4-slim
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV LANG en_US.UTF-8
RUN mkdir -p /app
WORKDIR /app
COPY package.json .
RUN yarn install
ENV PATH=/app/node_modules/.bin:$PATH
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["refmt", "--help"]
