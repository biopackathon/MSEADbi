\name{@PKGNAME@}
\alias{@PKGNAME@}
\title{Annotation package that provides Metabolite names linked to 
PathBank primary pathways}
\description{This data represents a collection of annotation packages
that can be used as a single object named as package name. This object
can be used with the standard four accessor method for all AnnotationDbi
objects.  Namely: \code{columns}, \code{keytypes}, and \code{select}.
Users are encouraged to read the vignette from the
\code{MSEADbi} package for more details.}
\author{@AUTHOR@}
\usage{@PKGNAME@}
\value{MSEA.XXX.pb.db package}
\examples{
library(AnnotationDbi)
library(@PKGNAME@)

cls <- columns(@PKGNAME@)
cls
kts <- keytypes(@PKGNAME@)
kt <- kts[2]
kts
ks <- head(keys(@PKGNAME@, keytype=kts[2]))
ks
res <- select(@PKGNAME@, keys=ks, columns=cls, keytype=kt)
head(res)

species(@PKGNAME@)
dbInfo(@PKGNAME@)
dbfile(@PKGNAME@)
dbschema(@PKGNAME@)
dbconn(@PKGNAME@)
mseaPackageName(@PKGNAME@)
mseaNomenclature(@PKGNAME@)
mseaListPathwaySubjects(@PKGNAME@)
mseaVersion(@PKGNAME@)
}
