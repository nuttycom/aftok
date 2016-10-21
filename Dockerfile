## Dockerfile for the Aftok environment
FROM       phusion/baseimage:0.9.19
MAINTAINER Kris Nuttycombe <kris@aftok.com>

## ensure locale is set during build
ENV LANG            C.UTF-8

# Base GHC/cabal install
RUN echo 'deb http://download.fpcomplete.com/ubuntu xenial main' > /etc/apt/sources.list.d/fpco.list && \
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 575159689BEFB442

# Install libpq-dev to enable postgresql-simple build
RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq-dev stack

# Install and build aftok-server dependencies
ADD ./aftok.cabal /opt/aftok/aftok.cabal
ADD ./stack.yaml  /opt/aftok/stack.yaml
ADD ./lib         /opt/aftok/lib
ADD ./server      /opt/aftok/server
ADD ./test        /opt/aftok/test
WORKDIR /opt/aftok

RUN stack setup
RUN stack install cpphs 

# Install and build aftok-server sources
RUN stack build

# Set up /etc/aftok volume for configuration information
RUN mkdir /etc/aftok
VOLUME ["/etc/aftok"]
ADD ./conf/aftok.cfg.example /etc/aftok/aftok.cfg
ENV AFTOK_CFG /etc/aftok/aftok.cfg

# This is the main shell script that starts the aftok server
RUN mkdir /etc/service/aftok
ADD ./docker/aftok-server.sh /etc/service/aftok/run

# Use baseimage-docker's init system.
CMD ["/sbin/my_init"]
