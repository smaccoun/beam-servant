#!/usr/bin/env bash

(stack build --fast --profile && env $(cat .env | xargs) stack exec -- api-exe +RTS -xc)
