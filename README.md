# Bahmni-Shiny
Shiny web app for exploring and viewing Bahmni data interactively

#### Vagrant Setup

* Make sure from `shiny-server` box there is a password-less ssh established to `bahmni box`.

``` 
sudo yum install epel-release
sudo yum install R
wget https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.5.3.838-rh5-x86_64.rpm
sudo yum install --nogpgcheck shiny-server-1.5.3.838-rh5-x86_64.rpm
sudo yum -y install libcurl libcurl-devel openssl-devel mysql-devel libjpeg-turbo-devel libpng-devel postgresql-devel nlopt nlopt-devel

cd /srv/shiny-server/
#remove all sample apps
sudo rm -rf /srv/shiny-server/*
#link bahmni-shiny folder to as a shiny app
sudo ln -s /vagrant/ bahmni-shiny

cd bahmni-shiny
#install all required packages
sudo R -f install_packages.R
```

* edit /etc/shiny-server/shiny-server.conf
    - change `run_as shiny` to `run_as vagrant` 
	- change 
		`site_dir /srv/shiny-server` to `app_dir /srv/shiny-server/bahmni-shiny`
	- add line `sanitize_errors false;` at the end of `location` section

#### Create Users table in db for the first time
	- cd /vagrant/
	- touch shiny.sqlite
	- sqlite3 shiny.sqlite
	- create table users(username varchar primary key, password varchar);
	- insert into users values('demo','$2a$12$aZyMtR.kaSqrpK2h/ID43utwz8bS6g.aovQW9z0/kvhlcnwYPfsfe');
	The above hash is the result of bcrypt::hashpw('12345') in R. Where 12345 is the password


