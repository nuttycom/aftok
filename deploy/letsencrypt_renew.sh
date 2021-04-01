#!/bin/bash

sudo docker run -it --rm \
    -v /opt/aftok/letsencrypt/data/letsencrypt:/data/letsencrypt \
    -v /opt/aftok/letsencrypt/etc/letsencrypt:/etc/letsencrypt \
    -v /opt/aftok/letsencrypt/var/lib/letsencrypt:/var/lib/letsencrypt \
    -v /opt/aftok/letsencrypt/var/log/letsencrypt:/var/log/letsencrypt \
    certbot/certbot renew
