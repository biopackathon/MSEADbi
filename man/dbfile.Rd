\name{dbfile}
\docType{methods}

\alias{generic,dbfile}
\alias{dbfile,MSEADb-method}
\alias{dbfile}

\title{A function to return the directory that the sqlite3 file stored}
\description{This function returns the directory that the sqlite file stored.}

\usage{dbfile(x)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
}
\value{
The directory path to the sqlite3 file
}
\author{Kozo Nishida}
\examples{
showMethods("dbfile")
}
