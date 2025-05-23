#!/bin/sh

cd jdk-11

bash ./configure --disable-warnings-as-errors --with-target-bits=32 --with-debug-level=fastdebug --with-jvm-features=graal --with-jvm-features=aot --with-boot-jdk=<path-to-boot-jdk>

make CONF=linux-x86-normal-server-fastdebug images
