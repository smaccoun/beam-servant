#!/usr/bin/env bash

(env $(cat .env.test | xargs) stack test )
