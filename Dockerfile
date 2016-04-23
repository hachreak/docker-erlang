FROM debian:jessie

MAINTAINER Leonardo Rossi <leonardo.rossi@studenti.unipr.it>

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y wget

RUN wget -O /tmp/erlang.deb https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i /tmp/erlang.deb && \
    rm /tmp/erlang.deb && \
    apt-get update && \
    apt-get install -y erlang

RUN groupadd -g 1000 -r erlang && useradd -d /home/erlang -m -u 1000 -r -g erlang erlang

# Define mountable directory for client.
VOLUME ["/var/www"]

WORKDIR /code

USER erlang

CMD ["erl"]
