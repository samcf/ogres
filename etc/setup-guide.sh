#!/bin/sh
set -o errexit -o nounset

# This script is not intended to actually be run, but serves as a guide to
# setup a new server instance by hand.

dst=$1

# update the list of available packages
apt update

# install certbot and create an ssl certificate. this creates a standalone
# web server which will respond to the authority challenge and generates
# the appropriate keys.
apt install certbot
certbot certonly --standalone -d ogres.app

# install nginx, copy the ogres.app nginx configuration to the appropriate
# location, and start the server
scp etc/ogres.app.conf "$dst":/usr/share/nginx
apt install nginx
nginx -s stop
nginx -c ogres.app.conf

# build the server uberjar locally and copy it to the server
clojure -A:uberjar -m hf.depstar.uberjar OgreServer.jar
scp OgreServer.jar "$dst":/srv/ogres.server

# copy the ogres.app service configuration to the server, enable and start
# the service with systemctl
apt install openjdk-18-jre-headless
scp etc/ogres.app.service "$dst":/etc/systemd/system
systemctl daemon-reload
systemctl restart ogres.app.service

# renew the ssl certificate by stopping the nginx service, running the certbot
# command to renew, and starting the nginx service again
nginx -s stop
sudo certbot renew
nginx -c ogres.app.conf

# setup grafana instrumentation by setting this environment variable
# for authentication and ensure that the grafana agent JAR exists
# within the same directory as the server JAR.
export OTEL_EXPORTER_OTLP_HEADERS="Authorization=Basic <Key>"
wget https://github.com/grafana/grafana-opentelemetry-java/releases/latest/download/grafana-opentelemetry-java.jar
scp grafana-opentelemetry-java.jar "$dst":/srv/ogres.server
