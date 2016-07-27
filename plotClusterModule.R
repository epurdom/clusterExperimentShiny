



plotClusterInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  
  
  
}

makePlotClustersCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    
    
  })
  
}

plotClustersHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotClusters()"),
    helpText("helptext here")
  )
}