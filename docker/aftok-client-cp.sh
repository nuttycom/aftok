#!/bin/bash

echo "Copying client build artifacts to mounted volume..."
cp -r /opt/aftok/client/$1/* /opt/aftok/client/dist-volume
echo "Client copy complete. The container will now shut down."
