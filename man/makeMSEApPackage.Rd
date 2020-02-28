\name{makeMSEApPackage}
\alias{makeMSEApPackage}
\title{Making MSEApDb packages from corresponding table as single data frame.}
\usage{
makeMSEApPackage(
    pkgname,
    data,
    metadata,
    organism,
    version,
    maintainer,
    author,
    destDir,
    license = "Artistic-2.0"
)
}
\arguments{
\item{pkgname}{What is the desired package name.}

\item{data}{Data frame contains GENEID (e.g., 100036770), MESHID
(e.g.D000465), CATEGORY (e.g., G), SOURCEID (pubmed id), and SOURCEDB (e.g.,
gendoo)}

\item{metadata}{Data frame contains metadata of the package}

\item{organism}{The name of the organism this package represents}

\item{version}{What is the version number for this package?}

\item{maintainer}{Who is the package maintainer? (must include email to be
valid)}

\item{author}{Who is the creator of this package?}

\item{destDir}{A path where the package source should be assembled.}

\item{license}{What is the license (and it's version)}
}
\value{
A special package to load an \link{MSEApDb} object.
}
\description{
\code{makeMSEApPackage} is a method that generates a package that will
load an appropriate \code{MSEApDb} object that will in turn point to existing
annotation packages.
}
\details{
The purpose of this method is to create a special package that will depend
on existing annotation packages and which will load a special \code{MSEApDb}
object that will allow proper dispatch of special select methods. These
methods will allow the user to easily query across multiple annotation
resources via information contained by the \code{MSEApDb} object. Because the
end result will be a package that treats all the data mapped together as a
single source, the user is encouraged to take extra care to ensure that the
different packages used are from the same build etc.
}
\examples{

## makeMSEApPackage enable users to construct
## user's own custom MSEAp annotation package

## this is test data which means the relationship between
## PathBank pathway IDs of Arabidopsis thaliana
## and its compound DB IDs (e.g., HMDB, CAS, etc...).
data(MSEAp.Ath.pb.db_DATA)
head(MSEAp.Ath.pb.db_DATA)

# We are also needed to prepare meta data as follows.
data(MSEAp.Ath.pb.db_METADATA)
MSEAp.Ath.pb.db_DATA

## sets up a temporary directory for this example
## (users won't need to do this step)
tmp <- tempfile()
dir.create(tmp) 

## makes an Organism package for Arabidopsis

MSEApDbi::makeMSEApPackage(
                pkgname = "MSEAp.Ath.pb.db", 
                data = MSEAp.Ath.pb.db_DATA, 
                metadata = MSEAp.Ath.pb.db_METADATA, 
                organism = "Arabidopsis thaliana", 
                version = "0.99.0", 
                maintainer = "Kozo Nishida <kozo.nishida@gmail.com>",
                author = "Kozo Nishida", 
                destDir = tmp, 
                license = "Artistic-2.0")

}
\seealso{
\code{\link{MSEApDb}}
}
\author{
Koki Tsuyuzaki
}
