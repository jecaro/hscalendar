# Start from the small stack image from FP Complete
FROM fpco/stack-build-small:lts-13.12 as builder

# Add the postgres lib
RUN apt-get update && apt-get install -y libpq-dev

# Install elm
RUN wget -O elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
RUN gunzip elm.gz
RUN chmod +x elm
RUN mv elm /usr/local/bin

# Create working directory
RUN mkdir /opt/build
WORKDIR /opt/build

# Copy source files and build the binaries
COPY . /opt/build
# Build the binaries
# --test for building the test suite
# --no-run-tests for not running the tests, we will run then on the next CI step
RUN stack build --test --no-run-tests --system-ghc
# Generate elm code
RUN stack exec elm-generator -- -o frontend/src
# Build the frontend
RUN make -C frontend

# After succesfull build copy the resulting binaries
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin
# And the test suite
RUN mv "$(stack path --dist-dir)/build/hscalendar-test/hscalendar-test" /opt/build/bin
# And the frontend files
RUN make -C frontend destdir=/opt/build/bin/frontend install

# Base image for stack-build-small
FROM ubuntu:16.04
RUN mkdir -p /opt/bin
WORKDIR /opt/bin

# Install lib gmp and postgres
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev \
  libpq-dev

# Copy the binaries from builder
COPY --from=builder /opt/build/bin .
EXPOSE 8081
CMD ["/opt/bin/hscalendar-server"]
