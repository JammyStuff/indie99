#!/usr/bin/env sh

docker run --name indie99-db --volumes-from indie99-dbdata -e POSTGRES_USER=indie99 -d postgres
