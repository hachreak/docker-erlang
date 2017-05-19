FROM debian:jessie

MAINTAINER Leonardo Rossi <leonardo.rossi@studenti.unipr.it>

RUN export DEBIAN_FRONTEND=noninteractive && \
    apt-get update && \
    apt-get install -y wget git build-essential

RUN wget -O /tmp/erlang.deb https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb && \
    dpkg -i /tmp/erlang.deb && \
    rm /tmp/erlang.deb && \
    apt-get update && \
    apt-get install -y erlang

RUN groupadd -g 1000 -r erlang && useradd -d /home/erlang -m -u 1000 -r -g erlang erlang

WORKDIR /tmp

USER erlang

# Install rebar3
RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    ./rebar3 local install && \
    echo "export PATH=\$PATH:\$HOME/.cache/rebar3/bin" >> $HOME/.bashrc

# Define mountable directory for client.
VOLUME ["/var/www"]

WORKDIR /var/www

CMD ["erl"]
