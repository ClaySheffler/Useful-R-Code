
## General functions to extract data from a database using either JDBC with driver or ODBC


# Using JDBC
getDBConnection_JDBC <- function(server, dbname, credentials, driver) {
  require(RJDBC)
  # Assumes a specific driver and jarfile for the connection string. Change here if neccessary
  drv <- RJDBC::JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", driver)
  if (!is.null(credentials)) {
    # Use explicit username and password
    username = credentials$username
    password = credentials$password
    con <- RJDBC::dbConnect(drv, 
                            paste0("jdbc:sqlserver://", server, "; databaseName=", dbname, ";integratedSecurity=false"),
                            username,
                            password)
  } else {
    # Use integrated security
    con <- RJDBC::dbConnect(drv, paste0("jdbc:sqlserver://", server, "; databaseName=", dbname, ";integratedSecurity=true"))
  }
  
  return(con)
}

dbQuery_RJDBC <- function(server, dbname, credentials, driver, query = NULL, table = NULL) {
  if (is.null(query) && is.null(table)) {
    return(NULL);
  }
  else if ( is.null(query) && !is.null(table) ) {
    query = paste0('SELECT * FROM ', table)
  }
  con = getDBConnection_JDBC(server, dbname, credentials, driver)
  df <- RJDBC::dbGetQuery(con, query)
  RJDBC::dbDisconnect(con)
  return(df)
}

getDBConnection_ODBC <- function(server, dbname) {
  require(RODBC)
  # Using ODBC
  connectionString = paste0("Driver=ODBC Driver 11 for SQL Server;Server=", server, "; Database=", dbname, ";Trusted_Connection=yes")
  con <- RODBC::odbcDriverConnect(connectionString)
  return(con)
}


dbQuery_ODBC <- function(server, dbname, query = NULL, table = NULL) {
  if (is.null(query) && is.null(table)) {
    return(NULL);
  }
  else if ( is.null(query) && !is.null(table) ) {
    query = paste0('SELECT * FROM ', table)
  }
  
  require(RODBC)
  con = getDBConnection_ODBC(server, dbname)
  df = RODBC::sqlQuery(con, query, stringsAsFactors=F, as.is=T) 
  RODBC::odbcClose(con)
  return(df)
}
