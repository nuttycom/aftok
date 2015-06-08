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

To start the server, you'll need to create an SSL X509 certificate that's 
used for encryption of cookies. We're not currently taking advantage of this,
since there's no browser-based UI, but without it the server will fail to start.

openssl req -x509 -newkey rsa:2048 -keyout conf/key.pem -out conf/cert.pem -days 365 -nodes

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

