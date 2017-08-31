library(properties)

host <- properties$host
identityFilePath <- properties$identityFilePath
sshUsername <- properties$sshUsername

mysqlDb <- properties$mysqlDbName
mysqlUser <- properties$mysqlUser
mysqlPassword <- properties$mysqlPassword
localMysqlPort <- properties$localMysqlPort

psqlDb <- properties$psqlDbName
psqlUser <- properties$psqlUser
psqlPassword <- properties$psqlPassword
localPsqlPort <- properties$localPsqlPort


getMysqlConnectionPool <- function() {
	if(host=="localhost") {
	  localhost <- "localhost"
	} else {
	  localhost <- "127.0.0.1"
	  sshMysqlTunnleCommand <- paste("ssh -v -f -o StrictHostKeyChecking=no -i ", identityFilePath, " -L ", localMysqlPort, ":localhost:3306 ", sshUsername, "@", host, " sleep 10", sep="")
	  system(sshMysqlTunnleCommand)
	}
	DBI::dbConnect(
	  drv = RMySQL::MySQL(),
	  dbname = mysqlDb,
	  host = localhost,
	  username = mysqlUser,
	  password = mysqlPassword,
	  port = strtoi(localMysqlPort)
	)
}

disconnectFromDb <- function(conn){
	DBI::dbDisconnect(conn)
}

getPsqlConnectionPool <- function() {
	if(host=="localhost") {
	  localhost <- "localhost"
	} else {
	  localhost <- "127.0.0.1"
	  sshPsqlTunnleCommand <- paste("ssh -v -f -o StrictHostKeyChecking=no -i ", identityFilePath, " -L ", localPsqlPort, ":localhost:5432 ", sshUsername, "@", host, " sleep 10", sep="")
	  system(sshPsqlTunnleCommand)
	}
	DBI::dbConnect(
	  drv = RPostgreSQL::PostgreSQL(),
	  dbname = psqlDb,
	  host = localhost,
	  user = psqlUser,
	  password = psqlPassword,
	  port = strtoi(localPsqlPort)
	)
}
