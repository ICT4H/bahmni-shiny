FROM bahmni/bahmni_centos67
RUN rpm -Uvh http://download.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
RUN yum install -y sudo git openssh-server openssh-clients tar wget yum-plugin-ovl  ; yum clean all
RUN yum install -y R
RUN wget https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.5.3.838-rh5-x86_64.rpm
RUN yum install -y --nogpgcheck shiny-server-1.5.3.838-rh5-x86_64.rpm
RUN yum -y install libcurl libcurl-devel openssl-devel mysql-devel libjpeg-turbo-devel libpng-devel postgresql-devel
RUN cd /srv/shiny-server/
RUN rm -rf /srv/shiny-server/*
RUN wget https://github.com/chethandeshpande/bahmni-shiny/archive/master.zip
RUN unzip master.zip
RUN mv bahmni-shiny-master bahmni-shiny
RUN cd bahmni-shiny
RUN R -f install_packages.R
RUN mv /etc/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf.bkp
RUN mv setup/shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN mv setup/*-shiny.sh /usr/bin/
RUN chmod +x /user/bin/*-shiny.sh
RUN chown shiny:shiny /bahmni-shiny
