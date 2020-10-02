\name{dbschema}
\docType{methods}

\alias{generic,dbschema}
\alias{dbschema,MSEADb-method}
\alias{dbschema}

\title{A function to return the schema of sqlite-database}
\description{This function returns the schema of sqlite-database.}

\usage{dbschema(x, file = "", show.indices = FALSE)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
\item{file}{\code{MSEADb} file}
\item{show.indices}{\code{MSEADb} show.indices}
}
\value{
The schema for constructing sqlite database file
}
\author{Kozo Nishida}
\examples{
showMethods("dbschema")
}
