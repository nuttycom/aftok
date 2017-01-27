Deployment
==========

At present, the aftok software is deployed on a DigitalOcean virtual machine.

The deployment architecture is relatively unsophisticated; both the
docker engine and a postgres server are running directly on the host VM,
and all other services are provided by containers. 

Configuration files for container-based services are stored on the 
host vm under `/opt/containers/<container-name>` and are mounted
into the appropriate containers as part of the `docker run` invocations
that are used to run the containers. A high-priority objective is to
simplify this setup by using `docker compose` to orchestrate the
containers, and to containerize the postgres server and data.

Docker Hub
----------

Configuration-free images containing the aftok server application is hosted on
dockerhub at nuttycom/aftok. To push a new version:

~~~{bash}

docker build -t nuttycom/aftok:$(git describe)

~~~


Manual Setup
------------

See `deploy/setup.sh` for a complete list of the operations that were
performed manually on the vm to get the aftok.com server up and running.
