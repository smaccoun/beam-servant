#!/usr/bin/env bash

docker stop test-db && docker rm test-db

docker run --name test-db  -p 7543:5432 -h 127.0.0.1 -e POSTGRES_DB=test_db --env-file .env.test.setup -d postgres

(env $(cat .env.test | xargs) stack test )
