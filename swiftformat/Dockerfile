# ================================
# Build image
# ================================
FROM swift:latest as build

LABEL maintainer="JacobCXDev <jacobcxdev@gmail.com>"

# Setup a build area
WORKDIR /build

# Clone and build SwiftFormat
RUN git clone https://github.com/nicklockwood/SwiftFormat .
RUN swift build -c release

# Switch to the staging area
WORKDIR /staging

# Copy main executable to staging area
RUN cp "$(swift build --package-path /build -c release --show-bin-path)/swiftformat" .

# ================================
# Run image
# ================================
FROM swift:slim

# Copy built executable and any staged resources from builder
COPY --from=build /staging /usr/local/bin

RUN mkdir -p /code
WORKDIR /code

# Run SwiftFormat
ENTRYPOINT [ "swiftformat" ]
CMD ["--help"]