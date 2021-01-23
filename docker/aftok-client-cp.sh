#!/bin/bash

echo "Copying client build artifacts to mounted volume..."
cp -r /opt/aftok/client/dist/* /opt/aftok/client/dist-volume
echo "Client copy complete. The container will now shut down."
