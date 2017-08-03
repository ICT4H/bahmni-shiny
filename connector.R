library(pool)
library(properties)

properties <- read.properties("app.properties")

dbName <- properties$database
host <- properties$host
dbUsername <- properties$dbUsername
dbPassword <- properties$dbPassword
identityFilePath <- properties$identityFilePath
localPort <- properties$localPort
sshUsername <- properties$sshUsername

if(host=="localhost") {
  localhost <- "localhost"
} else {
  localhost <- "127.0.0.1"
  sshTunnleCommand <- paste("ssh -v -f -o StrictHostKeyChecking=no -i ", identityFilePath, " -L ", localPort, ":localhost:3306 ", sshUsername, "@", host, " sleep 10", sep="")
  system(sshTunnleCommand)
}
getConnectionPool <- function() {
	pool <- dbPool(
	  drv = RMySQL::MySQL(),
	  dbname = dbName,
	  host = localhost,
	  username = dbUsername,
	  password = dbPassword,
	  port = strtoi(localPort)
	)
	return(pool)	
}
