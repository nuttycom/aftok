apt-get update
apt-get install -y postgresql
apt-get install -y postgresql-contrib
apt-get install -y git

addgroup admin
adduser nuttycom admin

# docker setup
wget -qO- https://get.docker.com/ | sh

mkdir -p /opt/containers/aftok-server
chown -R root:docker /opt/containers
chmod -R g+ws /opt/containers

git clone git@github.com:nuttycom/aftok.git
cp aftok/conf/aftok.cfg.example /opt/containers/aftok-server/aftok.cfg

# postgres setup
sudo -u postgres createuser aftok -d -E -P 
createdb -O aftok -h 127.0.0.1 -p 5432 -U aftok -W -E UTF8 aftok
sudo -u postgres psql -c 'create extension if not exists "uuid-ossp";' aftok

# SSL cert setup
openssl req -x509 -newkey rsa:2048 -keyout /opt/containers/aftok-server/key.pem -out /opt/containers/aftok-server/cert.pem -days 365 -nodes

docker run --name aftok-server -v /opt/containers/aftok-server:/etc/aftok:ro --net="host" -d nuttycom/aftok

mkdir -p /opt/containers/nginx
docker run --name aftok-nginx -v /opt/containers/nginx:/etc/nginx:ro --net="host" -d nginx

