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
makePlotCoClusteringCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("plotCoClustering(cE")
    code <- paste(")")
  })
  return(code)
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