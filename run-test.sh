#!/usr/bin/env bash

docker stop test-db
docker run --name test-db  -p 7543:5432 -h 127.0.0.1 --env-file .env.test -d postgres
(env $(cat .env.test | xargs) stack test )
