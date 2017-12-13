#!/bin/bash

/root/.local/bin/moo-postgresql upgrade --config-file=/etc/aftok/aftok-migrations.cfg
/opt/aftok/bin/aftok-server
