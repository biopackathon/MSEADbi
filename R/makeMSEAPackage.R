##
## This is for constracting original MSEA.XXX.pb.db packages by end-users
##
#' Making MSEADb packages from corresponding table as single data frame.
#' 
#' \code{makeMSEAPackage} is a method that generates a package that will
#' load an appropriate \code{MSEADb} object that will in turn point to existing
#' annotation packages.
#' 
#' The purpose of this method is to create a special package that will depend
#' on existing annotation packages and which will load a special \code{MSEADb}
#' object that will allow proper dispatch of special select methods. These
#' methods will allow the user to easily query across multiple annotation
#' resources via information contained by the \code{MSEADb} object. Because the
#' end result will be a package that treats all the data mapped together as a
#' single source, the user is encouraged to take extra care to ensure that the
#' different packages used are from the same build etc.
#' 
#' @param pkgname What is the desired package name.
#' @param data Data frame contains GENEID (e.g., 100036770), MESHID
#' (e.g.D000465), CATEGORY (e.g., G), SOURCEID (pubmed id), and SOURCEDB (e.g.,
#' gendoo)
#' @param metadata Data frame contains metadata of the package
#' @param organism The name of the organism this package represents
#' @param version What is the version number for this package?
#' @param maintainer Who is the package maintainer? (must include email to be
#' valid)
#' @param author Who is the creator of this package?
#' @param destDir A path where the package source should be assembled.
#' @param license What is the license (and it's version)
#' @return A special package to load an \link{MSEADb} object.
#' @author Koki Tsuyuzaki
#' @seealso \code{\link{MSEADb}}
#' @examples
#' 
#' ## makeMSEAPackage enable users to construct
#' ## user's own custom MSEA annotation package
#' 
#' ## this is test data which means the relationship between
#' ## PathBank pathway IDs of Arabidopsis thaliana
#' ## and its compound DB IDs (e.g., HMDB, CAS, etc...).
#' tmp <- tempdir()
#' ath <- system.file("extdata","MSEA.Ath.pb.db_DATA.csv",package="MSEADbi")
#' meta <- system.file("extdata","MSEA.Ath.pb.db_METADATA.csv",
#'     package="MSEADbi")
#' athDf <- read.csv(ath)
#' metaDf <- read.csv(meta)
#' # We need to avoid DOT from the column names (to query with the names)
#' names(athDf) <- gsub("\\.", "", names(athDf))
#' names(metaDf) <- gsub("\\.", "", names(metaDf))
#' 
#' makeMSEAPackage(pkgname = "MSEA.Ath.pb.db", data=athDf, metadata=metaDf,
#'     organism = "Arabidopsis thaliana", version = "0.99.0",
#'     maintainer = "Kozo Nishida <kozo.nishida@gmail.com>",
#'     author = "Kozo Nishida",
#'     destDir = tmp, license = "Artistic-2.0")
#' 
#' mseaPackageDir = paste(tmp, "MSEA.Ath.pb.db", sep="/")
#' install.packages(mseaPackageDir, repos=NULL, type="source")
#' 
#' @export makeMSEAPackage
#' 
makeMSEAPackage <- function(pkgname, data, metadata, organism, version,
    maintainer, author, destDir, license="Artistic-2.0"){
    .validateColNames1(data)
    .validateColNames2(metadata)
    template_path <- system.file("MSEAPkg-template", package="MSEADbi")
    symvals <- list(
        PKGTITLE=paste("An annotation package for the MSEADb object"),
        PKGDESCRIPTION=paste("Contains the MSEADb object",
            "to access data from several related annotation packages."),
        PKGVERSION=version,
        AUTHOR=author,
        MAINTAINER=maintainer,
        LIC=license,
        ORGANISM=organism,
        ORGANISMBIOCVIEW=gsub(" ","_",organism),
        PKGNAME=pkgname
    )
    .isSingleString <- function (x){
        is.character(x) && length(x) == 1L && !is.na(x)
    }
    if (any(duplicated(names(symvals))))
        stop("'symvals' contains duplicated symbols")
    is_OK <- vapply(symvals, .isSingleString, TRUE)
    if (!all(is_OK)) {
        bad_syms <- paste(names(is_OK)[!is_OK], collapse="', '")
        stop("values for symbols '", bad_syms, "' are not single strings")
    }
    createPackage(pkgname = pkgname,
        destinationDir = destDir,
        originDir = template_path,
        symbolValues = symvals,
        unlink = TRUE
    )
    template_sqlite <- paste0(system.file("DBschemas", package = "MSEADbi"),
        "/MSEA.XXX.pb.db.sqlite")
    dir.create(paste0(destDir, "/", pkgname, "/inst/extdata"),
        showWarnings = FALSE, recursive = TRUE)
    dest_sqlitepath <- paste0(destDir, "/", pkgname, "/inst/extdata/")
    file.copy(from = template_sqlite, to = dest_sqlitepath,
        overwrite=TRUE)
    old_dest_sqlite <- paste0(dest_sqlitepath, "MSEA.XXX.pb.db.sqlite")
    new_dest_sqlite <- paste0(dest_sqlitepath, pkgname, ".sqlite")
    file.rename(from = old_dest_sqlite, to = new_dest_sqlite)
    conn <- dbConnect(SQLite(), dbname = new_dest_sqlite)
    dbWriteTable(conn, name="METADATA", value=metadata, overwrite=TRUE)
    dbWriteTable(conn, name="DATA", value=data, overwrite=TRUE)
    dbDisconnect(conn)
}

.validateColNames1 <- function(data){
    if(ncol(data) != 15){
        stop("Data should has 15 columns!")
    }
    if(colnames(data)[1] != "PathBankID"){
        stop("Please specify the name of 1st column as 'PathBank ID'")
    }
    if(colnames(data)[2] != "PathwayName"){
        stop("Please specify the name of 1st column as 'Pathway Name'")
    }
    if(colnames(data)[3] != "PathwaySubject"){
        stop("Please specify the name of 1st column as 'Pathway Subject'")
    }
    if(colnames(data)[4] != "MetaboliteID"){
        stop("Please specify the name of 1st column as 'Metabolite ID'")
    }
}

.validateColNames2 <- function(metadata){
    if(ncol(metadata) != 2){
        stop("Meta data should has 2 columns!")
    }
    if(colnames(metadata)[1] != "NAME"){
        stop("Please specify the name of 1st column as 'NAME'")
    }
    if(colnames(metadata)[2] != "VALUE"){
        stop("Please specify the name of 2nd column as 'VALUE'")
    }
}
