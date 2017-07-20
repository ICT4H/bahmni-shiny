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

sshTunnleCommand <- paste("ssh -v -f -o StrictHostKeyChecking=no -i ", identityFilePath, " -L ", localPort, ":localhost:3306 ", sshUsername, "@", host, " sleep 10", sep="")
system(sshTunnleCommand)

getConnectionPool <- function() {
	pool <- dbPool(
	  drv = RMySQL::MySQL(),
	  dbname = dbName,
	  host = "127.0.0.1",
	  username = dbUsername,
	  password = dbPassword,
	  port = strtoi(localPort)
	)
	return(pool)	
}
