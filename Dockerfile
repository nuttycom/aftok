## Dockerfile for the Aftok environment
FROM       phusion/baseimage:0.9.16
MAINTAINER Kris Nuttycombe <kris@aftok.com>

## ensure locale is set during build
ENV LANG            C.UTF-8

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
            cabal-install-1.22 \
            ghc-7.10.2 \
            happy-1.19.5 \
            alex-3.1.4 \
            zlib1g-dev \
            libtinfo-dev \
            libsqlite3-0 \
            libsqlite3-dev \
            ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV PATH /root/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.2/bin:/opt/happy/1.19.5/bin:/opt/alex/3.1.4/bin:$PATH

ADD ./aftok.cabal /opt/aftok/aftok.cabal
RUN cabal update
RUN cd /opt/aftok && cabal sandbox init && cabal install --only-dependencies -j4

ADD ./lib /opt/aftok/lib
ADD ./server /opt/aftok/server

# Set up /etc/aftok volume for configuration information
RUN mkdir /etc/aftok
VOLUME ["/etc/aftok"]
ADD ./conf/aftok.example.cfg /etc/aftok/aftok.cfg
ENV AFTOK_CFG /etc/aftok/aftok.cfg

# This is the main shell script that starts the aftok server
RUN mkdir /etc/services/aftok
ADD ./docker/aftok-server.sh /etc/services/aftok/run

# Use baseimage-docker's init system.
CMD ["/sbin/my_init"]
