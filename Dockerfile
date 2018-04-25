FROM  fpco/stack-build:lts-11.2 as build



RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc


FROM ubuntu:16.04

# install database dependencies
RUN ["apt-get", "update"]
RUN ["apt-get", "-y", "install", "libpq-dev"]
RUN ["apt-get", "-y", "install", "pkg-config"]
RUN ["apt-get", "-y", "install", "libpcre3", "libpcre3-dev"]
RUN ["apt-get", "-y", "install", "build-essential"]

# PLACE PREINSTALLED BINARY IN IMAGE
RUN mkdir -p /opt/myapp
ARG BINARY_PATH
WORKDIR /opt/myapp
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
# NOTICE THIS LINE
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-11.2/8.2.2/bin .
CMD ["/opt/myapp/api-exe"]

EXPOSE 8080
