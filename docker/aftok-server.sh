#!/usr/bin/execlineb -P

s6-setuidgid daemon
# /root/.local/bin/moo-postgresql upgrade --config-file=/etc/aftok/aftok-migrations.cfg
backtick AFTOK_CFG { echo "/etc/aftok/aftok.cfg" }
/opt/aftok/bin/aftok-server
