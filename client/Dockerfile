FROM       ubuntu:focal
MAINTAINER Kris Nuttycombe <kris@aftok.com>

ENV LANG            C.UTF-8
ENV TZ              America/Denver
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# Install build tools & library dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libtinfo5 nodejs npm netbase git

RUN apt-get install -y --no-install-recommends ca-certificates
RUN update-ca-certificates

RUN mkdir -p /opt/aftok/client
WORKDIR /opt/aftok/client

ADD ./client/package.json /opt/aftok/client/package.json

RUN npm install
ENV PATH="./node_modules/.bin:${PATH}"

# Add static assets
ADD ./aftok.com/src/assets /opt/aftok/aftok.com/src/assets
ADD ./client/dev /opt/aftok/client/dev
RUN mkdir -p /opt/aftok/client/prod && \
    ln -s /opt/aftok/aftok.com/src/assets /opt/aftok/client/prod/assets

# Add purescript build config & sources
ADD ./client/spago.dhall /opt/aftok/client/spago.dhall
ADD ./client/packages.dhall /opt/aftok/client/packages.dhall
ADD ./client/src /opt/aftok/client/src

RUN npm run build-prod

# Add dist-volume directory for use with docker-compose sharing
# of client executables via volumes.
ADD ./docker/aftok-client-cp.sh /opt/aftok/
RUN chmod 700 /opt/aftok/aftok-client-cp.sh
RUN mkdir /opt/aftok/client/dist-volume
