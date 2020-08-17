## Dockerfile for the Aftok environment
FROM       ubuntu:focal
MAINTAINER Kris Nuttycombe <kris@aftok.com>

## ensure locale is set during build
ENV LANG            C.UTF-8
ENV TZ              America/Denver
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install build tools & library dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential autotools-dev autoconf dh-autoreconf \
    g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg curl \
    libpq-dev libsqlite3-dev libssl-dev \
    libsecp256k1-dev

RUN apt-get install -y --no-install-recommends ca-certificates

RUN update-ca-certificates
ADD ./docker/install_stack.sh /root/
RUN /bin/sh /root/install_stack.sh

RUN /usr/local/bin/stack upgrade --force-download

# Install ghc globally so that we don't have to reinstall it
# whenever we change stack.yaml or aftok.cabal
ADD ./docker/global-stack.yaml /root/.stack/global-project/stack.yaml
RUN /root/.local/bin/stack --resolver lts-13.9 setup

# Globally install database migrations tool
RUN /root/.local/bin/stack install dbmigrations
RUN /root/.local/bin/stack install dbmigrations-postgresql

# Set up /etc/aftok volume for mounting configuration from the host system
RUN mkdir /etc/aftok
VOLUME ["/etc/aftok"]
ENV AFTOK_CFG /etc/aftok/aftok.cfg

# Install and build aftok-server dependencies
RUN mkdir -p /opt/aftok
WORKDIR /opt/aftok

ADD ./aftok.cabal /opt/aftok/aftok.cabal
ADD ./stack.yaml  /opt/aftok/stack.yaml

# Build dependencies
RUN /root/.local/bin/stack setup
RUN /root/.local/bin/stack install cpphs 
RUN /root/.local/bin/stack build --only-dependencies -j1

ADD ./lib         /opt/aftok/lib
ADD ./daemon      /opt/aftok/daemon
ADD ./server      /opt/aftok/server
ADD ./test        /opt/aftok/test
ADD ./migrations  /opt/aftok/migrations

# build and install and aftok-server sources
RUN /root/.local/bin/stack install

# add the s6-overlay init process
# RUN mkdir -p /etc/services.d/aftok
# RUN mkdir -p /etc/fix-attrs.d/
# 
# ADD https://github.com/just-containers/s6-overlay/releases/download/v1.21.8.0/s6-overlay-amd64.tar.gz /tmp/
# RUN tar xzf /tmp/s6-overlay-amd64.tar.gz -C / --exclude="./bin" && \
#     tar xzf /tmp/s6-overlay-amd64.tar.gz -C /usr ./bin
# 
# ADD ./docker/aftok-server.sh /etc/services.d/aftok/run
# ADD ./docker/fix-attrs.d/* /etc/fix-attrs.d/
# 
# ENTRYPOINT ["/init"]

ENTRYPOINT ["/opt/aftok/bin/aftok-server"]

