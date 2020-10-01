##
## Definition of Classes
##

# Reference class
.MSEADb <- setRefClass("MSEADb", contains="AnnotationDb")

## Constructor
MSEADb <- function(pkgname){
    ## Inherit class, Instantiation
    .dbconn <- dbConnect(
        SQLite(),
        paste0(
            system.file(c("inst", "extdata"), package=pkgname),
            paste0("/", pkgname, ".sqlite")
        )
    )
    obj <- .MSEADb$new(conn=.dbconn, packageName=pkgname)
    return(obj)
}
