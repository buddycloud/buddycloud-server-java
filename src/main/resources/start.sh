#!/bin/bash

OPTS=""

if [ -z "$DATABASE" ]; then
    OPTS=" $OPTS -DDATABASE=\"$DATABASE\""
fi

java $OPTS -jar target/channelserver-jar-with-dependencies.jar