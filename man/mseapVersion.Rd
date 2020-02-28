\name{mseapVersion}
\docType{methods}

\alias{generic,mseapVersion}
\alias{mseapVersion,MSEApDb-method}
\alias{mseapVersion}

\title{A function to return the MeSH version of package}
\description{This function returns the version of MSEAp used in the package.}

\usage{mseapVersion(x)}

\arguments{
\item{x}{\code{MSEApDb} object such as \code{MSEAp.Ath.pb.db}}
}

\value{
    \code{mseapVersion} returns a data.frame.
}

\author{Kozo Nishida}
\examples{
library("MSEAp.Ath.pb.db")
mseapVersion(MSEAp.Ath.pb.db)
}