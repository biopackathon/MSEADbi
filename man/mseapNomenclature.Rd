\name{nomenclature}
\docType{methods}

\alias{generic,mseapNomenclature}
\alias{mseapNomenclature,MSEApDb-method}
\alias{mseapNomenclature}

\title{A function to return the scientific name of package}
\description{This function returns the scientific name of species used in the package.}

\usage{mseapNomenclature(x)}

\arguments{
\item{x}{\code{MSEApDb} object such as \code{MSEAp.Ath.pb.db}}
}

\value{
  \code{mseapNomenclature} returns a "character".
}

\author{Kozo Nishida}
\examples{
library("MSEAp.Ath.pb.db")
mseapNomenclature(MSEAp.Ath.pb.db)
}