##
## This is for MSAp.XXX.eg.db
##

.loadMSEApDbiPkg <- function (pkgname) {

    ## Inherit class, Instantiation
    obj <- MSEApDb(pkgname)

    ## Export object
    ns <- asNamespace(pkgname)
    assign(pkgname, obj, envir=ns)
    namespaceExport(ns, pkgname)
}
