Local Builds with Nix
=====================

The simplest way to get the server building is currently to use [nix](https://nixos.org/manual/nixos/stable/).

Once you've installed nix, from the root of the project, run:

~~~bash
nix build
~~~

This will download and compile all dependencies, the aftok source, and create a server docker image (located in the `result`
file). To load the resulting docker file into docker, run `docker load < result`. For convenience, this set of steps is
already defined for the `build-server-image` target in the `Makefile`.

Local Development with docker-compose
=====================================

The easiest way to run local aftok infrastructure is using Docker and
docker-compose. Several steps are necessary to set up your local environment
for development.

Docker Permissions
------------------

The first step is to ensure that you have proper permissions to access and
execute docker commands. On OSX, if you've installed Docker Desktop then this
should already be the case. On Linux, you can either add your user to the
`docker` group, or you can use `sudo` for commands.

Configuration Files
-------------------

A number of configuration files are required for docker-compose to be able
to run all of the necessary containers successfully. You should create
a `local` directory, which will have the following contents:

~~~
local
├── conf
│   ├── nginx
│   │   ├── mime.types
│   │   └── nginx.conf
│   ├── nginx-certs
│   │   ├── aftok.crt
│   │   └── aftok.key
│   └── server
│       ├── aftok.bip70-chain.cert.pem
│       ├── aftok.bip70.key.pem
│       ├── aftok.cfg
│       ├── aftok-migrations.cfg
│       └── snap-site-key
└── db-dumps
    └── aftok.sample.plsql
~~~

Sample default versions of each of these files can be found in the `conf`
directory; you can simply use the following to set up your local environment:

~~~bash
mkdir local
cp -r conf local
~~~

Database Initialization
-----------------------

When you first set up your local docker environment for aftok development, the
database that is created by `docker-compose up` will not be initialized.  The
easiest way to get it set up is to bootstrap from an existing database dump. 

First, you'll need to start the servers. Expect aftok-server and aftok-nginx to
both fail to start; they won't work properly until the database is initialized,
but that's okay.

Also, if you have a copy of postgres already running at localhost:5432 you may
need to change the exposed port in docker-compose.yml so as to avoid conflicts.

~~~bash
docker-compose up
docker ps
~~~

At this point, the `aftok-db` container should be the only one that's running;
the other two will have failed on startup.

Initializing With an Existing Dump
----------------------------------

Assuming that you have such a dump at `local/postgres/db-dumps/aftok.dump`, use the
`deploy/dbinit.sh` script to initialize the database. The postgres user's password 
can be specified in the docker-compose file. 

~~~bash
./deploy/dbinit.sh local/postgres/db-dumps/aftok.dump
~~~

Initializing From Scratch
-------------------------

We first need to create the PostgreSQL accounts, which you can do with the
`dbinit.sh` script:

~~~bash
./deploy/dbinit.sh
~~~

Restarting the Application
--------------------------

Now, you should be able to shut down docker-compose using ^C and 
restart it with `docker-compose up`.

Database Configuration
----------------------

All database DDL state is handled using the Haskell dbmigrations tool.

Once all the containers are up, you'll need to run the existing database
migrations as follows:

~~~bash
stack install dbmigrations-postgresql
moo-postgresql upgrade --config-file ./local/server/conf/aftok-migrations.cfg
~~~

New migrations can be created with:

~~~bash
moo-postgresql new --config-file ./local/server/conf/aftok-migrations.cfg kebab-case-descriptive-name
~~~
