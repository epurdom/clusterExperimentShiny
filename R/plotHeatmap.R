
#Skeleton of Plot Heatmap

#' @rdname InternalModules
#' @export
plotClusterInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  tagList(
  )
}






#' @rdname InternalModules
#' @export
plotHeatmapHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotHeatmap()"),
    helpText("helptext here")
  )
}