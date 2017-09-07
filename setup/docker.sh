docker build --rm -t bahmni/bahmni_centos67 .
docker run -it -d -p 3838:3838 -v /bahmni-shiny/:/var/lib/bahmni-shiny/ bahmni/bahmni_centos67 /bin/bash