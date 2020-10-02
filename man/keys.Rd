\name{keys}
\docType{methods}

\alias{generic,keys}
\alias{keys,MSEADb-method}
\alias{keys}

\title{A function defined by AnnotationDbi to return the keys}
\description{This function returns the keys.}

\usage{keys(x, keytype, ...)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
\item{keytype}{\code{MSEADb} What kind of key user want to specify}
\item{...}{\code{MSEADb} Other parameter}
}
\value{
The keys for using select functions
}
\author{Kozo Nishida}
\examples{
showMethods("keys")
}
