#plotCoClustering skeleton
plotCoClusteringInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  tagList(
  )
}


makePlotCoClusteringCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("plotCoClustering(cE")
    code <- paste(")")
  })
  return(code)
}






plotCoClusteringHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotCoClustering()"),
    helpText("helptext here")
  )
}