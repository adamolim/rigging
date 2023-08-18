# Clean console and environnement


rm(list=ls())

# Fonction to source the rmd file

runAllChunks <- function(rmd, envir=globalenv()){
    tempR <- tempfile(tmpdir = ".", fileext = ".R")
    on.exit(unlink(tempR))
    knitr::purl(rmd, output=tempR)
    sys.source(tempR, envir=envir)
}

runAllChunks("rowingdata.Rmd")

