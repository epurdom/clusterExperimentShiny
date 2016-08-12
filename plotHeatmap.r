plotClusterInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  tagList(
  )
}



makePlotHeatmapCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("plotHeatmap(cE")
    
    code <- paste(code, ")")
  })
  return(code)
}


plotHeatmapHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotHeatmap()"),
    helpText("helptext here")
  )
}