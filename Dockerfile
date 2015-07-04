## Dockerfile for the Aftok environment
FROM       phusion/baseimage:0.9.16
MAINTAINER Kris Nuttycombe <kris@aftok.com>

## ensure locale is set during build
ENV LANG            C.UTF-8

# Base GHC/cabal install
RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends cabal-install-1.22 ghc-7.10.2 happy-1.19.5 alex-3.1.4 \
            zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV PATH /root/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:$PATH

# Install libpq-dev to enable postgresql-simple build
RUN apt-get update && \
    apt-get install -y --no-install-recommends wget && \
    echo 'deb http://apt.postgresql.org/pub/repos/apt/ trusty-pgdg main' > /etc/apt/sources.list.d/pgdg.list && \
    wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - && \
    apt-get update && \
    apt-get install -y --no-install-recommends libpq-dev

# Install and build aftok-server dependencies
ADD ./aftok.cabal /opt/aftok/aftok.cabal
WORKDIR /opt/aftok
RUN cabal update
RUN cabal install cpphs 
RUN cabal install --only-dependencies -j4 

# Install and build aftok-server sources
ADD ./lib         /opt/aftok/lib
ADD ./server      /opt/aftok/server
RUN cabal configure && cabal build aftok-server

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
