\name{dbInfo}
\docType{methods}

\alias{generic,dbInfo}
\alias{dbInfo,MSEADb-method}
\alias{dbInfo}

\title{A function to return the database information}
\description{This function returns the database information,
which is METADATA table in sqlite file.}

\usage{dbInfo(x)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
}
\value{
The information of MSEA.XXX.pb.db package
}
\author{Kozo Nishida}
\examples{
showMethods("dbInfo")
}
