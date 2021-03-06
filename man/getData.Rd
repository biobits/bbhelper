% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dbhelper.R
\name{getData}
\alias{getData}
\title{R Function für generating a custom colorpallette of length x}
\usage{
getData(sqlquery, dbase = "ucchscience")
}
\arguments{
\item{sqlquery}{:character vector - The SQL-Query}

\item{dbase}{:character vector - Name of Database as specified in odbc.ini to parse internally into getDBConnect}
}
\value{
an DBIResult
}
\description{
R Function für generating a custom colorpallette of length x
}
\examples{
result<-getData("Select * from Patients",dbase= "mydatabase")



}
\author{
Stefan Bartels, \email{email@biobits.eu}
}
