#plotCoClustering skeleton
#' @rdname InternalModules
#' @export
plotCoClusteringInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  tagList(
  )
}



#' @rdname InternalModules
#' @export
plotCoClusteringHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotCoClustering()"),
    helpText("helptext here")
  )
}