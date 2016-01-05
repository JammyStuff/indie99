#!/usr/bin/env sh

APP_DIR=$(pwd)

docker run --link indie99-db -v ${APP_DIR%/*}:/app -w /app/indie99 -p 8001:8001 --rm -it indie99-dev /bin/bash
