# Run container for client
FROM debian:latest

ENV DEBIAN_FRONTEND noninteractive

# Update system
RUN apt-get update && apt-get -y install erlang && \
    groupadd -g 1000 erlang && \
    useradd -u 1000 -g erlang -d /var/www/ erlang

# Define mountable directory for client.
VOLUME ["/var/www"]

# Expose port.
# EXPOSE 8000

USER erlang

# Execute python server
WORKDIR /var/www/
CMD ["erl"]
