FROM mcr.microsoft.com/dotnet/core/sdk
RUN dotnet tool install -g fantomas-tool
ENV PATH="/root/.dotnet/tools:${PATH}"
RUN mkdir -p /code
WORKDIR /code
#COPY files /
CMD ["fantomas", "--help"]