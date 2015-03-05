# Run container for client
FROM debian:latest

MAINTAINER Leonardo Rossi <leonardo.rossi@studenti.unipr.it>

ENV DEBIAN_FRONTEND noninteractive

# Update system
RUN apt-get update && \
    apt-get -y install erlang sudo && \
    apt-get -y autoremove && \
    apt-get -y clean && \
    apt-get -y autoclean && \
    groupadd -g 1000 erlang && \
    useradd -u 1000 -g erlang -G sudo -d /var/www/ erlang -s /bin/bash && \
    echo 'erlang  ALL=(ALL:ALL) NOPASSWD: ALL' >> /etc/sudoers && \
    service sudo restart

# Define mountable directory for client.
VOLUME ["/var/www"]

USER erlang

# Execute python server
WORKDIR /var/www/
CMD ["erl"]
