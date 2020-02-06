FROM fpco/stack-build:lts-14.22 as build
RUN mkdir /opt/build
RUN mkdir /opt/build/bin
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc --copy-bins --local-bin-path /opt/build/bin
FROM ubuntu:19.04
RUN mkdir -p /opt/mu-docker-example
ARG BINARY_PATH
WORKDIR /opt/mu-docker-example
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE
COPY --from=build /opt/build/bin .
# COPY other-files /opt/mu-docker-example/other-files
EXPOSE 8080
CMD ["/opt/mu-docker-example/mu-docker-example"]
