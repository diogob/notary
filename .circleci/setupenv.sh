#!/usr/bin/env bash
apt-get update
apt-get install -y postgresql-client

DBNAME=${DBNAME=notary_test}
DBHOST=${DBHOST=localhost}
DBUSER=${DBUSER=notary}
DBURI="postgres://${DBUSER}@${DBHOST}/${DBNAME}"

psql ${DBURI} -c '\set ON_ERROR_STOP' -a < postgresql-migrations/schema.sql

stack install --system-ghc --resolver=lts-14.7 hlint