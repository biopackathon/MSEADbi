\name{keytypes}
\docType{methods}

\alias{generic,keytypes}
\alias{keytypes,MSEADb-method}
\alias{keytypes}

\title{A function to return the columns that user can specified
in select function}
\description{This function returns the columns that user can specified in
select function.}

\usage{keytypes(x)}

\arguments{
\item{x}{\code{MSEADb} object such as \code{MSEA.Ath.pb.db}}
}
\value{
The columns that user can specify as key
}
\author{Kozo Nishida}
\examples{
showMethods("keytypes")
}
