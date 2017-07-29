FROM haskell

RUN apt-get install software-properties-common
RUN apt-key adv --recv-keys --keyserver keyserver.ubuntu.com 0xcbcb082a1bb943db
RUN add-apt-repository 'deb [arch=amd64,i386,ppc64el] http://mirror.netinch.com/pub/mariadb/repo/10.0/debian jessie main'
RUN apt-get update
RUN apt-get install -y libmariadbclient-dev

VOLUME /var/app
RUN cd /var/app

WORKDIR /var/app
