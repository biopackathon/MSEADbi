##
## Definition of Classes
##

# Reference class
.MSEApDb <- setRefClass("MSEApDb", contains="AnnotationDb")

## Constructor
MSEApDb <- function(pkgname){
    ## Inherit class, Instantiation
    .dbconn <- dbConnect(
        SQLite(),
        paste0(
            system.file(c("inst", "extdata"), package=pkgname),
            paste0("/", pkgname, ".sqlite")
        )
    )
    obj <- .MSEApDb$new(conn=.dbconn, packageName=pkgname)
    return(obj)
}
