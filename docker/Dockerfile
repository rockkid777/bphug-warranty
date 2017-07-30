FROM haskell

RUN apt-get update
RUN apt-get install -y libmysqlclient-dev
RUN apt-get install -y libghc-pcre-light-dev 
RUN apt-get upgrade -y pkg-config

VOLUME /var/app
RUN cd /var/app

WORKDIR /var/app
