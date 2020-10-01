##
## This is for MSAp.XXX.eg.db
##

.loadMSEADbiPkg <- function (pkgname) {

    ## Inherit class, Instantiation
    obj <- MSEADb(pkgname)

    ## Export object
    ns <- asNamespace(pkgname)
    assign(pkgname, obj, envir=ns)
    namespaceExport(ns, pkgname)
}
