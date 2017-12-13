FROM bahmni/bahmni_centos67
RUN rpm -qa | grep -q epel-release || rpm -Uvh http://download.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
RUN yum install -y sudo git openssh-server openssh-clients tar wget yum-plugin-ovl R libcurl libcurl-devel openssl-devel mysql-devel libjpeg-turbo-devel libpng-devel postgresql-devel ; yum clean all
RUN wget https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.5.3.838-rh5-x86_64.rpm && \
	yum install -y --nogpgcheck shiny-server-1.5.3.838-rh5-x86_64.rpm && \
	rm -rf /srv/shiny-server/* && \
 	cd /srv/shiny-server/ && \
 	wget https://github.com/ICT4H/bahmni-shiny/archive/master.zip && \
 	unzip master.zip && \
 	mv bahmni-shiny-master bahmni-shiny && \
 	chown shiny:shiny -R bahmni-shiny && \
 	rm master.zip && \
	cd /srv/shiny-server/bahmni-shiny && \
	R -f install_packages.R

RUN mv /etc/shiny-server/shiny-server.conf /etc/shiny-server/shiny-server.conf.bkp && \
	cd /srv/shiny-server/bahmni-shiny && \
	mv setup/shiny-server.conf /etc/shiny-server/shiny-server.conf && \
	mv setup/*-shiny.sh /usr/bin/ && \
	chmod +x /usr/bin/start-shiny.sh && \
	chmod +x /usr/bin/stop-shiny.sh && \
	su shiny -c 'ssh-keygen -f $HOME/.ssh/id_rsa -t rsa -N ""'
