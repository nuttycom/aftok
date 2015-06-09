Development Installation
========================

First, you'll need a local install of postgres. This is because the
postgresql-simple package requires some of the postgres command-line tools
and/or libraries in order to build.

You'll want to create a new postgres database for the aftok project, and a
user with superuser permissions on that database. Then, you can use the
script sql/aftok-pg.sql to create the initial state of your database.

psql -h localhost -f sql/aftok-pg.sql aftok aftok

Then, the usual setup steps apply:

cabal sandbox init
cabal configure
cabal install cpphs //for some reason the thyme library won't find this dependency on its own
cabal install --only-dependencies --enable-tests
cabal build
cabal test

If using nix, instead you can do the following:

cd <project_root>
cabal2nix --shell . > shell.nix
nix-shell -I ~ --command 'cabal configure --enable-tests'

Next, copy the example config file into place and edit it to provide the port you want
the service to run on, and your postgres database connection information.

cp conf/aftok.cfg.example conf/aftok.cfg
vi conf/aftok.cfg

Now, when you do 'cabal run aftok-server' it should actually start up and run.

There are a few shell scripts in <project_root>/scripts that provide basic
functionality for creating a project, creating a user, and starting and
stopping your clock. Right now these are hardcoded with project identifiers and
a bitcoin address that are local to a test blockchain on my system. Speaking of
which, you'll also eventually want to install bitcoind, although at the moment
basically nothing related to the bitcoin infrastructure is working anyway so
it's not immediately necessary.

Docker Installation
===================

The Aftok server application is now deployable under Docker. Here's how you can
go about running the server in a local docker container:

docker build -t <yourid>/aftok:<version> .

for example,

docker build -t nuttycom/aftok:0.1 .

This will give you an image ID that you can then run. The container is set up
to provide a mountable filesystem volume at /etc/aftok where you can put your
configuration files. You will need to copy your aftok.cfg file and your two
.pem files to somewhere local (say, a docker-conf directory) and then edit the
aftok.cfg file make sense in the context of the container. For example, here's 
an aftok.cfg set up for docker use:

~~~
port = 8000

siteKey = "/etc/aftok/snap-site-key"

db {
  host = "localhost"
  port = 5432
  user = "aftok"
  pass = ""
  db = "aftok"

  # Nmuber of distinct connection pools to maintain.  The smallest acceptable
  # value is 1.
  numStripes = 1
  
  # Number of seconds an unused resource is kept open.  The smallest acceptable
  # value is 0.5 seconds.
  idleTime = 5
  
  # Maximum number of resources to keep open per stripe.  The smallest
  # acceptable value is 1.
  maxResourcesPerStripe = 20
}
~~~

To run the container in "development" mode, it's useful to leave open a TTY, so that you
can see output from stdout:

docker run -i -t --net="host" -v /home/nuttycom/projects/aftok/docker-conf:/etc/aftok <image_id>

Or, you can daemonize the container, as we will do in production:

docker run -d --net="host" -v /home/nuttycom/projects/aftok/docker-conf:/etc/aftok <image_id>

In both of these cases, I'm simply using the --net="host" switch to allow the container
access to 'localhost' on the host machine, so that I don't have to explicitly
configure where postgres is running.
