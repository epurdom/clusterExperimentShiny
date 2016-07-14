library(shiny)
library(ggvis)
source("global.R")
shinyServer(function(input, output, session) {
  datafile <- callModule(csvFile, "parameters",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    datafile()
  })
  
  dimReduce <- callModule(testing, "parameters",
                          stringsAsFactors = FALSE)
  
  output$dimReduceCode <- renderText({
    dimReduce()
  })
  
  clusterManyCode <- callModule(makeCode, "parameters",
                                stringsAsFactors = FALSE)
  
  output$clusterManyCode <- renderText({
    clusterManyCode()
  })
  
})





