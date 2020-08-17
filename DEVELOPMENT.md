Local Development with docker-compose
=====================================

When you first set up your local docker environment for aftok development, the
database that is created by `docker-compose up` will not be initialized.  The
easiest way to get it set up is to bootstrap from an existing database dump. 

First, you'll need to start the servers. Expect aftok-server and aftok-nginx to
both fail to start; they won't work properly until the database is initialized,
but that's okay.

Also, if you have a copy of postgres already running at localhost:5432 you may
need to change the exposed port in docker-compose.yml so as to avoid conflicts.

If you're using docker on OSX, the `sudo` in the lines below (which is required
on linux) should be omitted.

~~~bash
sudo docker-compose up
sudo docker ps
~~~

At this point, the `aftok-db` container should be the only one that's running;
the other two will have failed on startup.

Assuming that you have such a dump at `local/db-dumps/aftok.dump`, use the
following commands to initialize the database. The postgres user's password is
specified in the docker-compose file.

~~~bash
createuser -h localhost -U postgres -W -P aftok
createdb -h localhost -U postgres -W -O aftok aftok 
psql -h localhost -U aftok -W aftok < local/db-dumps/aftok.dump
~~~

Now, you should be able to shut down docker-compose using ^C and 
restart it with `docker-compose up`.

Once all the containers are up, you'll need to run the database migrations
as follows:

~~~bash
stack install dbmigrations-postgresql
moo-postgresql upgrade --config-file ./local/conf/server/aftok-migrations.cfg
~~~
