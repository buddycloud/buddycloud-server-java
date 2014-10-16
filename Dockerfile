################################################################################
# Build a dockerfile for buddycloud-server-java
# Based on ubuntu
################################################################################

FROM dockerfile/java

EXPOSE 3000

RUN apt-get update
RUN apt-get upgrade -y

RUN git clone https://github.com/buddycloud/buddycloud-server-java.git
RUN cd buddycloud-server-java && mvn package
ADD ./src/main/resources/log4j.properties .
ADD ./src/main/resources/start.sh .

CMD /bin/bash start.sh