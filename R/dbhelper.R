# Helpers for DataBase Connections rewritten using DBI and odbc pacckage




##########################################################################################################################################################################################################
## Making a Connect to odbc datasource
##########################################################################################################################################################################################################
#' R Function für generating a custom colorpallette of length x
#'
#' @param dbase :character vector - Name of ODBC-Connection as specified in odbc.ini
#'
#' @param dbuser :character vector - Username for database
#'
#' @param dbpwd :character vector - DB Password
#'
#' @param useconfig :Boolean - If True the functions tries to read username and password from a config YAML File (default: "/etc/odbc_security.yml")
#'
#' @param configfile :character vector - Path to config-YAML file with stored credentials
#'
#' @return an odbc database connection
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' con<-getDBConnect(dbase= "mydatabase")
#'
#' con <-getDBConnect("mydatabase",dbuser="User",dbpwd="password",useconfig=F)
#'
#'
#'
#'@export
getDBConnect<-function(dbase="ucchscience",dbuser=NULL,dbpwd=NULL,useconfig=T,configfile=NULL) {
  if(is.null(dbuser)){
    dbuser<-"user"
  }

  if(is.null(dbpwd)){
    dbpwd<-"pwd"
  }

  if(useconfig==T){
    if(is.null(configfile)){
      configfile <- "/etc/odbc_security.yml"
    }
    try({

      conf <- config::get(file = configfile)
      dbuser<-conf$user
      dbpwd<-conf$password
    }
  )
  }


  return(DBI::dbConnect(odbc::odbc(),dbase,UID = dbuser,PWD = dbpwd))
}


##########################################################################################################################################################################################################
## Helper for Parsing an SQL-Query to a database
##########################################################################################################################################################################################################
#' R Function für generating a custom colorpallette of length x
#'
#' @param sqlquery :character vector - The SQL-Query
#'
#' @param dbase :character vector - Name of Database as specified in odbc.ini to parse internally into getDBConnect
#'
#' @return an DBIResult
#'
#' @author Stefan Bartels, \email{email@biobits.eu}
#'
#' @examples
#' result<-getData("Select * from Patients",dbase= "mydatabase")
#'
#'
#'
#'@export
getData<-function(sqlquery,dbase="ucchscience"){
    db<-getDBConnect(dbase)
    return(odbc::dbGetQuery(db,sqlquery))
    #odbcClose(db)
    DBI::dbDisconnect(db)
  }
