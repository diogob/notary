#!/usr/bin/env bash
apt-get update
apt-get install -y postgresql-client

DBNAME=${DBNAME=notary_test}
DBHOST=${DBHOST=localhost}
DBUSER=${DBUSER=notary}
DBURI="postgres://${DBUSER}@${DBHOST}/${DBNAME}"

psql ${DBURI} < postgresql-migrations/schema.sql

stack install --resolver=lts-14.7 hlint