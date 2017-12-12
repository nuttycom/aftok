#!/bin/bash

/root/.local/bin/moo upgrade -c /etc/aftok/aftok-migrations.cfg
/opt/aftok/bin/aftok-server
