# Bahmni-Shiny
Shiny web app for exploring and viewing Bahmni data interactively

This guide will tell how to setup bahmni-shiny with docker on centos 6.
#### Setup Docker

    rpm -iUvh http://dl.fedoraproject.org/pub/epel/6/x86_64/epel-release-6-8.noarch.rpm
    yum update -y
    yum -y install docker-io


#### Prepare the local image
Download the image for bahmni-shiny app from docker hub.
It will have the basic setup for bahmni-shiny, you need to customize it according your need.
After customization we will commit it as the base image for bahmni-shiny for our use.
	
	service docker start
	docker run -it mddubey/bahmni-shiny-v1 /bin/bash
	--setup properties file
	--Setup the passwordless ssh for shiny. copy the content of /home/.ssh/id_rsa.pub and append to /home/bahmni_vagrant/.ssh/authorized_keys on the bahmni machine (Assuming bahmni_support is the ssh user in properties)
	-- exit from docker container
	docker ps -a (Copy the container-id)
	docker commit <container-id>  shiny-local
	docker rm <container-id>


#### Setup the shared folder
All the data related to the application, will be stored in a folder which will be shared with the docker container.

	cd /var/lib/
	mkdir bahmni-shiny
	cd bahmni-shiny
	mkdir preferences
	mkdir plugins
	--put all the plugin folders in above plugins folder
	touch shiny.sqlite
	sqlite3 shiny.sqlite
	create table users(username varchar primary key, password varchar);
	insert into users values('demo','$2a$12$aZyMtR.kaSqrpK2h/ID43utwz8bS6g.aovQW9z0/kvhlcnwYPfsfe');
	
The above hash is the result of bcrypt::hashpw('12345') in R. Where 12345 is the password
	
#### Create a container with the locally created image
    docker run --name shiny-app -it -v /var/lib/bahmni-shiny/:/bahmni-shiny/ -p 3838:3838 shiny-local /bin/bash
        --inside container
	    chown -R shiny:shiny /bahmni-shiny/
	    exit
#### Start the container & shiny-server
    docker start shiny-app
    docker exec shiny-app /bin/bash -c "start-shiny.sh"
    
#### Upgrading the docker image
To get new features added in the bahmni-shiny app, you will need to upgrade the local image to the latest development version.

    docker start shiny-app
    docker attach shiny-app
        --inside the container
        cd /srv/shiny-server/
        wget https://github.com/chethandeshpande/bahmni-shiny/archive/master.zip
        unzip master.zip
        cp bahmni-shiny/app.properties bahmni-shiny-master/
        rm -rf bahmni-shiny
        mv bahmni-shiny-master/ bahmni-shiny
        chown shiny:shiny -R bahmni-shiny/
        rm -f master.zip
        cd bahmni-shiny
        R -f install_packages.R
        exit
    --commit updated container as base image
    docker commit shiny-app shiny-local
    
    

