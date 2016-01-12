#!/usr/bin/env sh

mv boss.config boss.config.orig
mv boss.test.config boss.config
./init-dev.sh
mv boss.config boss.test.config
mv boss.config.orig boss.config
