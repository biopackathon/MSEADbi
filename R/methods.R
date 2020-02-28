##
## Definition of Methods
##

## helper for vector dividing
.div <- function(x, d=1) {
  delta <- ceiling(length(x) / d)
  y <- lapply(seq_len(d), function(i){
    as.vector(na.omit(x[((i-1)*delta+1):(i*delta)]))
  })
  return(y)
}

setMethod("show",
          "MSEApDb",
          function(object){
            print("##### class ####")
            print(class(object))
            print("##### connection #####")
            print(object$conn)
            print("##### package name #####")
            print(object$packageName)
          }
)

# columns
setMethod("columns",
          "MSEApDb",
          function(x) {
            return(dbGetQuery(dbconn(x),"PRAGMA TABLE_INFO(DATA);")$name)
          }
)

# keytypes
setMethod("keytypes",
          "MSEApDb",
          function(x) {
            return(dbGetQuery(dbconn(x),"PRAGMA TABLE_INFO(DATA);")$name)
          }
)

# keys
setMethod("keys",
          "MSEApDb",
          function(x, keytype){
            query <- paste0("SELECT ", keytype, " FROM DATA;")
            k     <- unlist(unique(dbGetQuery(x$conn, query)))
            names(k) <- NULL
            return(k)
          }
)

# select
setMethod("select",
          "MSEApDb",
          function(x, keys, columns, keytype){
            c <- paste(columns, collapse=",")
            keys <- paste0('"', keys, '"')
            ke <- paste(keytype, keys, sep ="=")
            if(length(ke) >= 1000) {
              no.chunk <- ceiling(length(keys)/500)
              ke_loc <- .div(seq_along(ke), no.chunk)
              kee <- lapply(ke_loc, function(x){
                paste(ke[x], collapse=" OR ")
              })
            }else{
              kee <- paste(ke, sep="", collapse=" OR ")
            }
            
            # SQLs
            kk <- lapply(kee, function(i) {
              query <- paste0("SELECT ", c, " FROM DATA WHERE ", i)
              dbGetQuery(x$conn, query)
            })
            kk <- do.call(rbind, kk)
            return(unique(kk))
          }
)


## dbconn
setMethod("dbconn",
          "MSEApDb",
          function(x){
            return(x$conn)
          }
)

## dbfile
setMethod("dbfile",
          "MSEApDb",
          function(x){
            return(
              paste0(
                system.file(c("inst", "extdata"), package=x$packageName),
                paste0("/", x$packageName, ".sqlite")
              )
            )
          }
)

## dbschema
setMethod("dbschema",
          "MSEApDb",
          function(x){
            return(dbGetQuery(x$conn, "SELECT * FROM sqlite_master;")$sql)
          }
)

## dbInfo
setMethod("dbInfo",
          "MSEApDb",
          function(x){
            return(dbGetQuery(x$conn, "SELECT * FROM METADATA;"))
          }
)

## species
setMethod("species",
          "MSEApDb",
          function(object) {
            return(dbGetQuery(object$conn,
                              'SELECT value FROM METADATA where name = "SPECIES";')[1,])
          }
)

## mseapPackageName
setMethod("mseapPackageName",
          "MSEApDb",
          function(x){
            return(x$packageName)
          }
)

## mseapNomenclature
setMethod("mseapNomenclature",
          "MSEApDb",
          function(x) {
            return(dbGetQuery(x$conn,
                              'SELECT value FROM METADATA where name = "ORGANISM";')[1,])
          }
)

## mseapListPathwaySubjects
setMethod("mseapListPathwaySubjects",
          "MSEApDb",
          function(x) {
            return(dbGetQuery(x$conn, 'SELECT DISTINCT PathwaySubject FROM DATA;'))
          }
)

## mseapVersion
setMethod("mseapVersion",
          "MSEApDb",
          function(x){
            return(dbGetQuery(x$conn,
                              'SELECT * FROM METADATA where name = "MSEApVERSION";')[1,])
          }
)