.onLoad <- function(libname, pkgname){
    cat("Warning! An environment named 'appGlobal' will be created in your global environment by this package to keep track of global variables needed by the shiny app.")
 }