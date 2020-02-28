\name{MSEApDb-class}
\docType{class}

\alias{MSEApDb-class}
\alias{class:MSEApDb}
\alias{MSEApDb}

% select and select related methods
\alias{columns}
\alias{keytypes}
\alias{keys}
\alias{select}
\alias{dbconn}
\alias{dbfile}
\alias{dbschema}
\alias{dbInfo}
\alias{species}

\alias{columns,MSEApDb-method}
\alias{keytypes,MSEApDb-method}
\alias{keys,MSEApDb-method}
\alias{select,MSEApDb-method}
\alias{dbconn,MSEApDb-method}
\alias{dbfile,MSEApDb-method}
\alias{dbschema,MSEApDb-method}
\alias{dbInfo,MSEApDb-method}
\alias{species,MSEApDb-method}

\title{MSEApDb objects}
\description{
  \code{MSEApDb} is the simple class for providing the relationship between Entrez gene IDs and MeSH IDs. It provides the database connection and easily accessible with \code{columns}, \code{keytypes}, \code{keys} and \code{select}. Some users may use additional functions such as \code{dbconn}, \code{dbfile}, \code{dbschema}, \code{dbInfo}, and \code{species} for much complex data acquisition.

  \code{columns} shows which kinds of data can be returned for the
  \code{MSEApDb} object.

  \code{keytypes} allows the user to discover which keytypes can be
  passed in to \code{select} or \code{keys} and the \code{keytype}
  argument.

  \code{keys} returns keys for the database contained in the
  \code{MSEApDb} object .  This method is already documented in the keys manual page but is mentioned again here because it's usage with
  \code{select} is so intimate.  By default it will return the primary
  keys for the database, but if used with the \code{keytype} argument,
  it will return the keys from that keytype.

  \code{select} will retrieve the data as a data.frame based on
  parameters for selected \code{keys}, \code{columns}, and \code{keytype} arguments.

  \code{dbconn} returns the connection with database in the package.

  \code{dbfile} returns the absolute path sqlite file is saved.

  \code{dbschema} returns the database schema.

  \code{dbInfo} returns the many meta information about the package.

  \code{species} returns the species name.

}
\usage{
  columns(x)
  keytypes(x)
  keys(x, keytype, ...)
  select(x, keys, columns, keytype, ...)
  dbconn(x)
  dbfile(x)
  dbschema(x, file = "", show.indices = FALSE)
  dbInfo(x)
  species(object)
}
\arguments{
  \item{x}{the \code{MSEApDb} object. But in practice this will mean an object derived from an \code{MSEApDb} object such as a \code{MeSH.Hsa.eg.db}, \code{MeSH.Mmu.eg.db} or many other MeSH.XXX.eg.db (XXX means abbreviation of species name).}

  \item{object}{same as x}

  \item{keys}{the keys to select records for from the database.  All possible keys are returned by using the \code{keys} method.}

  \item{columns}{the columns or kinds of things that can be retrieved from the database. As with \code{keys}, all possible columns are returned by using the \code{columns} method.}

  \item{keytype}{the keytype that matches the keys used. For the
    \code{select} methods, this is used to indicate the kind of ID being used with the keys argument. For the \code{keys} method this is used to indicate which kind of keys are desired from \code{keys}}

  \item{...}{other arguments.}

  \item{file}{The \code{file} argument must be a connection, or a character string naming the file to print to (see the \code{file} argument of the \code{\link[base:cat]{cat}} function for the details).}

  \item{show.indices}{The CREATE INDEX statements are not shown by default. Use \code{show.indices=TRUE} to get them.}
}

\value{
  \code{keys}, \code{columns}, \code{keytypes}, \code{dbfile}, \code{dbInfo}, and \code{species} each return a character vector or possible values. \code{select} and \code{dbschema} each return a data.frame. \code{dbconn} returns database connection.
}

\author{Koki Tsuyuzaki}

\seealso{
  \code{\link[DBI:dbConnect]{dbConnect}}
}

\examples{

# # load a package that creates an MSEApDb object
# library(MSEAp.Ath.pb.db)
# MSEAp.Ath.pb.db

# ## then the methods can be used on this object.
# cls <- columns(MSEAp.Ath.pb.db)
# cls
# kts <- keytypes(MSEAp.Ath.pb.db)
# kt <- kts[2]
# kts
# ks <- head(keys(MSEAp.Ath.pb.db, keytype=kts[2]))
# ks
# res <- select(MSEAp.Ath.pb.db, keys=ks, columns=cls, keytype=kt)
# head(res)

# dbconn(MSEAp.Ath.pb.db)
# dbfile(MSEAp.Ath.pb.db)
# dbschema(MSEAp.Ath.pb.db)

# dbInfo(MSEAp.Ath.pb.db)
# species(MSEAp.Ath.pb.db)
}