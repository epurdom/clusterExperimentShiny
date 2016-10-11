#' Run the shiny app for clusterExperiment package
#' 
#' This function runs the shiny app that corresponds to the clusterExperiment
#' package.
#' 
#' @details Typing \code{runClusterExperiment()} at the command prompt (after
#'   loading the \code{clusterExpShiny} library) will cause a browser to open
#'   with the shiny app. 
#' @export
runClusterExperiment<-function(){
    packageName<-"clusterExperimentShinyApp"
    appDir <- system.file("shinyApp", package = packageName)
    if (appDir == "") {
        stop(paste("Could not find example directory. Try re-installing",packageName,"."), call. = FALSE)
    }
    shiny::runApp(appDir, display.mode = "normal")
}
