test_that("generated annotation package works", {
    #remove.packages("MSEA.Ath.pb.db")
    #Sys.setenv(R_INSTALL_STAGED = FALSE)
     
    library(AnnotationDbi)
    library(MSEADbi)
    tmp <- tempdir()
    
    ath <- system.file("extdata","MSEA.Ath.pb.db_DATA.csv",package="MSEADbi")
    meta <- system.file("extdata","MSEA.Ath.pb.db_METADATA.csv",
            package="MSEADbi")
    athDf <- read.csv(ath)
    metaDf <- read.csv(meta)
    # We need to avoid DOT from the column names (to query with the names)
    names(athDf) <- gsub("\\.", "", names(athDf))
    names(metaDf) <- gsub("\\.", "", names(metaDf))
    
    makeMSEAPackage(pkgname = "MSEA.Ath.pb.db", data=athDf, metadata=metaDf,
                     organism = "Arabidopsis thaliana", version = "0.99.0",
                     maintainer = "Kozo Nishida <kozo.nishida@gmail.com>",
                     author = "Kozo Nishida",
                     destDir = tmp, license = "Artistic-2.0")
    
    mseaPackageDir = paste(tmp, "MSEA.Ath.pb.db", sep="/")
    install.packages(mseaPackageDir, repos=NULL, type="source",
                     INSTALL_opts = c('--no-lock'))
    
    library(MSEADbi)
    library(MSEA.Ath.pb.db)
    load(MSEA.Ath.pb.db)
    
    ids <- c('SMP0012018', 'SMP0012019')
    result <- select(MSEA.Ath.pb.db, ids,
                     c("MetaboliteID", "CAS", "HMDBID", "ChEBIID", "KEGGID"),
                     "PathBankID")
    
    testthat::expect_is(result, "data.frame")
    testthat::expect_equal(10, sum(result$CAS == ""))
  
})
