test_that("multiplication works", {
    #remove.packages("MSEAp.Ath.pb.db")
    #Sys.setenv(R_INSTALL_STAGED = FALSE)
    
    library(AnnotationDbi)
    library(MSEApDbi)
    tmp <- tempdir()
    
    ath <- system.file("extdata","MSEAp.Ath.pb.db_DATA.csv",package="MSEApDbi")
    meta <- system.file("extdata","MSEAp.Ath.pb.db_METADATA.csv",
            package="MSEApDbi")
    athDf <- read.csv(ath)
    metaDf <- read.csv(meta)
    # We need to avoid DOT from the column names (to query with the names)
    names(athDf) <- gsub("\\.", "", names(athDf))
    names(metaDf) <- gsub("\\.", "", names(metaDf))
    
    makeMSEApPackage(pkgname = "MSEAp.Ath.pb.db", data=athDf, metadata=metaDf,
                     organism = "Arabidopsis thaliana", version = "0.99.0",
                     maintainer = "Kozo Nishida <kozo.nishida@gmail.com>",
                     author = "Kozo Nishida",
                     destDir = tmp, license = "Artistic-2.0")
    
    mseapPackageDir = paste(tmp, "MSEAp.Ath.pb.db", sep="/")
    install.packages(mseapPackageDir, repos=NULL, type="source",
                     INSTALL_opts = c('--no-lock'))
    
    library(MSEApDbi)
    library(MSEAp.Ath.pb.db)
    load(MSEAp.Ath.pb.db)
    
    ids <- c('SMP0012018', 'SMP0012019')
    result <- select(MSEAp.Ath.pb.db, ids,
                     c("MetaboliteID", "CAS", "HMDBID", "ChEBIID", "KEGGID"),
                     "PathBankID")
    
    testthat::expect_is(result, "data.frame")
    testthat::expect_equal(10, sum(result$CAS == ""))
  
})
