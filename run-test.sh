#!/usr/bin/env bash

docker stop test-db && docker rm test-db

docker run --name test-db  -p 7543:5432 -h 127.0.0.1  --env-file .env.test.setup -d postgres

docker inspect test-db

sleep 10

(env $(cat .env.test | xargs) stack test )
