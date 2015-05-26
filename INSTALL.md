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
used for encryption of cookies and so forth:

mkdir local
cd local
openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -days 365 -nodes
cd <project_root>
echo 'sslCert = "local/cert.pem"' > aftok.cfg
echo 'siteKey = "local/key.pem"' >> aftok.cfg

If you want to run on a port other than 8000, you can set 'port = 12345' in that
aftok.cfg file.

Next, we'll want to run the server in order to autogenerate the files that need
to be edited to configure postgres:

cabal run aftok-server

I can't remember whether this will succeed or fail to start, but in any case
once it has run you should be able to find the file
snaplets/postgresql-simple/devel.cfg

Edit this file to set the database connection information:

host = "localhost"
port = 5432
user = "aftok"
pass = "???"
db = "aftok"

Now, when you do 'cabal run aftok-server' it should actually start up and run.

There are a few shell scripts in <project_root>/scripts that provide basic
functionality for creating a project, creating a user, and starting and
stopping your clock. Right now these are hardcoded with project identifiers and
a bitcoin address that are local to a test blockchain on my system. Speaking of
which, you'll also eventually want to install bitcoind, although at the moment
basically nothing related to the bitcoin infrastructure is working anyway so
it's not immediately necessary.


