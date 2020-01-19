FROM mcr.microsoft.com/dotnet/core/sdk
LABEL maintainer="Jozef Hollý <j2.00ghz@gmail.com>"
ENV PATH="/root/.dotnet/tools:${PATH}"
ENV FANTAMOS_VERSION=3.3.0
RUN dotnet tool install \
  --global fantomas-tool \
  --version "$FANTAMOS_VERSION"
RUN mkdir -p /code
WORKDIR /code
CMD ["fantomas", "--help"]
