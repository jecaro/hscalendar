# Start from the small stack image from FP Complete
FROM fpco/stack-build-small:lts-13.12 as builder

# Add the postgres lib
RUN apt-get update && apt-get install -y libpq-dev

# Create working directory
RUN mkdir /opt/build
WORKDIR /opt/build

# Copy source files and build the binaries
COPY . /opt/build
RUN stack build --system-ghc

# After succesfull build copy the resulting binaries
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# Base image for stack-build-small
FROM ubuntu:16.04
RUN mkdir -p /opt/executable
WORKDIR /opt/executable

# Install lib gmp and postgres
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  libpq-dev

# Copy the binaries from builder
COPY --from=builder /opt/build/bin .
EXPOSE 8081
CMD ["/opt/executable/hscalendar-server"]
