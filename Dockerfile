################################################################################
# Build a dockerfile for buddycloud-server-java
# Based on ubuntu
################################################################################

FROM dockerfile/java

MAINTAINER Lloyd Watkin <lloyd@evilprofessor.co.uk>

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y maven

RUN git clone https://github.com/buddycloud/buddycloud-server-java.git
RUN cd buddycloud-server-java && mvn package
ADD ./src/main/resources/log4j.properties .
ADD ./src/main/resources/start.sh .

CMD /bin/bash start.sh