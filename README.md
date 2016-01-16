indie99: The Indie Web Blog with Nine 9s
========================================

indie99 is an indie web blog platform built in [Erlang](http://www.erlang.org/) using the [Chicago Boss](http://chicagoboss.org/) framework.

Getting Started
---------------

Copy the sample config, and change the necessary values at the bottom (in the `INDIE99 CONFIGURATION` section):

    cp boss.config.sample boss.config

Developing
----------

The easiest way to get started when developing is to use Docker. A Dockerfile is provided in `docker/`. To create the appropriate Docker image:

    docker build -t indie99-dev .

You can then get everything running using the provided scripts:

    ./docker-startdbdata.sh
    ./docker-startdb.sh
    ./docker-startshell.sh

The last script provides an interactive bash shell. To run the development server:

    ./init-dev.sh

You'll need to manually run the database migrations:

    boss_migrate:run(indie99).

Testing
-------

Firstly, create the test database:

    yum install postgresql
    psql -h indie99-db -U indie99 indie99 < indie99-dev.sql

Then, run the migrations:

    ./starttestshell.sh
    boss_migrate:run(indie99).

Finally, you can run the tests:

    make eunit
