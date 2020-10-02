\name{dbconn}
\docType{methods}

\alias{generic,dbconn}
\alias{dbconn,MSEADb-method}
\alias{dbconn}

\title{A function to return the connection to sqlite3 file}
\description{This function returns the connection to sqlite file.}

\usage{dbconn(x)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
}
\value{
The database connection to sqlite3 file
}
\author{Kozo Nishida}
\examples{
showMethods("dbconn")
}
