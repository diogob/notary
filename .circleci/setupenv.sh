#!/usr/bin/env bash
apt-get update
apt-get install -y postgresql-client

stack install --system-ghc --resolver=lts-13.29 hlint

DBNAME=${DBNAME=coinberry_test}
DBHOST=${DBHOST=localhost}
DBUSER=${DBUSER=coinberry}
DBURI="postgres://${DBUSER}@${DBHOST}/${DBNAME}"

psql ${DBURI} -c '\set ON_ERROR_STOP' -a < postgresql-migrations/schema.sql