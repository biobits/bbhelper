% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{subset.validlevels}
\alias{subset.validlevels}
\title{Subset a data.frame by group y where each level has to be larger than size x}
\usage{
\method{subset}{validlevels}(x, group, mincounts)
}
\arguments{
\item{x}{data.frame: The data.frame to subset}

\item{group}{character vector: the columnname of the data frame to build groups from}

\item{mincounts}{numeric vector: the minimal size allowed for a group zo stay in the dataframe}
}
\value{
dataframe
}
\description{
Subset a data.frame by group y where each level has to be larger than size x
}
\examples{
ID1<-c(2,3,45,343,34,8,77,88,90)
ID2<-c("A","A","B","C","C","C","D","D","D")
df<-data.frame(ID1,ID2)
df<-subset.validlevels(df,"ID2",3)

}
\author{
Stefan Bartels, \email{email@biobits.eu}
}
