#!/bin/bash

OPTS=""

if [ "$DATABASE" != "" ]; then
    OPTS=" $OPTS -DDATABASE=\"$DATABASE\""
fi

cd buddycloud-server-java
java $OPTS -jar target/channelserver-jar-with-dependencies.jar