##
## This is for constracting original MSEAp.XXX.pb.db packages by end-users
##
#' Making MSEApDb packages from corresponding table as single data frame.
#' 
#' \code{makeMSEApPackage} is a method that generates a package that will
#' load an appropriate \code{MSEApDb} object that will in turn point to existing
#' annotation packages.
#' 
#' The purpose of this method is to create a special package that will depend
#' on existing annotation packages and which will load a special \code{MSEApDb}
#' object that will allow proper dispatch of special select methods. These
#' methods will allow the user to easily query across multiple annotation
#' resources via information contained by the \code{MSEApDb} object. Because the
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
#' @return A special package to load an \link{MSEApDb} object.
#' @author Koki Tsuyuzaki
#' @seealso \code{\link{MSEApDb}}
#' @examples
#' 
#' ## makeMSEApPackage enable users to construct
#' ## user's own custom MSEAp annotation package
#' 
#' ## this is test data which means the relationship between
#' ## PathBank pathway IDs of Arabidopsis thaliana
#' ## and its compound DB IDs (e.g., HMDB, KEGG, CAS, etc...).
#' data(MSEAp.Ath.pb.db_DATA)
#' head(MSEAp.Ath.pb.db_DATA)
#' 
#' # We are also needed to prepare meta data as follows.
#' data(MSEAp.Ath.pb.db_METADATA)
#' MSEAp.Ath.pb.db_DATA
#' 
#' ## sets up a temporary directory for this example
#' ## (users won't need to do this step)
#' tmp <- tempfile()
#' dir.create(tmp) 
#' 
#' ## makes an Organism package for Arabidopsis
#' 
#' MSEApDbi::makeMSEApPackage(pkgname = "MSEAp.Ath.pb.db", 
#'                             data = MSEAp.Ath.pb.db_DATA, 
#'                             metadata = MSEAp.Ath.pb.db_METADATA, 
#'                             organism = "Arabidopsis thaliana", 
#'                             version = "0.99.0", 
#'                             maintainer = "Kozo Nishida <kozo.nishida@gmail.com>",
#'                             author = "Kozo Nishida", 
#'                             destDir = tmp, 
#'                             license = "Artistic-2.0")
#' 
#' @export makeMSEApPackage
#' 
makeMSEApPackage <- function(pkgname, data, metadata, organism, version,
    maintainer, author, destDir, license="Artistic-2.0"){

    # Validate of data
    .validateColNames1(data)
    .validateColNames2(metadata)

    ## there should only be one template
    template_path <- system.file("MSEApPkg-template", package="MSEApDbi")

    ## We need to define some symbols in order to have the
    ## template filled out correctly.
    symvals <- list(
        PKGTITLE=paste("An annotation package for the MSEApDb object"),
        PKGDESCRIPTION=paste("Contains the MSEApDb object",
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

    ## Should never have duplicates
    if (any(duplicated(names(symvals))))
        stop("'symvals' contains duplicated symbols")
    ## All symvals should by single strings (non-NA)
    is_OK <- vapply(symvals, .isSingleString, TRUE)
    if (!all(is_OK)) {
        bad_syms <- paste(names(is_OK)[!is_OK], collapse="', '")
        stop("values for symbols '", bad_syms, "' are not single strings")
    }

    ## create Package structure
    createPackage(pkgname = pkgname,
        destinationDir = destDir,
        originDir = template_path,
        symbolValues = symvals,
        unlink = TRUE
    )

    # # copy vignette
    # .pathRmd <- function(){
    #     LIBPATHS = .libPaths()
    #     MSEAPATH = sapply(LIBPATHS, function(x){
    #         file.exists(paste0(x, "/MSEApDbi/doc/MSEApDbi.Rnw"))
    #     })
    #     MSEAPATH = names(MSEAPATH[which(MSEAPATH)])
    #     if(length(MSEAPATH) != 0){
    #         paste0(MSEAPATH[1], "/MSEApDbi/doc/MSEApDbi.Rnw")
    #     }else{
    #         stop("The library path is not found!\n")
    #     }
    # }
    # 
    # dir.create(paste0(destDir, "/", pkgname, "/vignettes/"),
    #     showWarnings = FALSE, recursive = TRUE)
    # template_rnw <- .pathRmd()
    # new_rnw <- unlist(read.delim(template_rnw, header=FALSE, stringsAsFactor=FALSE))
    # new_rnw <- gsub("MSEApDbi", pkgname, new_rnw)
    # sink(paste0(destDir, "/", pkgname, "/vignettes/", pkgname, ".Rnw"))
    # for(i in seq_along(new_rnw)){
    #     cat(paste0(new_rnw[i], "\n"))
    # }
    # sink()

    ## move template to dest
    template_sqlite <- paste0(system.file("DBschemas", package = "MSEApDbi"),
        "/MSEAp.XXX.pb.db.sqlite")
    dir.create(paste0(destDir, "/", pkgname, "/inst/extdata"),
        showWarnings = FALSE, recursive = TRUE)
    dest_sqlitepath <- paste0(destDir, "/", pkgname, "/inst/extdata/")
    file.copy(from = template_sqlite, to = dest_sqlitepath,
        overwrite=TRUE)

    ## rename
    old_dest_sqlite <- paste0(dest_sqlitepath, "MSEAp.XXX.pb.db.sqlite")
    new_dest_sqlite <- paste0(dest_sqlitepath, pkgname, ".sqlite")
    file.rename(from = old_dest_sqlite, to = new_dest_sqlite)

    # ## connection
    conn <- dbConnect(SQLite(), dbname = new_dest_sqlite)

    ## insert metadata into moved sqlite database
    dbWriteTable(conn, name="METADATA", value=metadata, overwrite=TRUE)

    ## insert data and metadata into moved sqlite database
    dbWriteTable(conn, name="DATA", value=data, overwrite=TRUE)

    # disconnection
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
