\name{select}
\docType{methods}

\alias{generic,select}
\alias{select,MSEADb-method}
\alias{select}

\title{select function defined by AnnotationDbi-class}
\description{This function returns the corresponding rows that user specified}

\usage{select(x, keys, columns, keytype, ...)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
\item{keys}{\code{MSEADb} keys vector}
\item{columns}{\code{MSEADb} columns the user want to retrieve}
\item{keytype}{\code{MSEADb} What kind of key the user want to specify}
\item{...}{\code{MSEADb} Other parameter}
}
\value{
select function defined by AnnotationDbi package
}
\author{Kozo Nishida}
\examples{
showMethods("select")
}
