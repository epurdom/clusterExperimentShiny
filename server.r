library(shiny)
library(ggvis)
source("global.R")
shinyServer(function(input, output, session) {
  datafile <- callModule(dataFile, "parameters",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    datafile()
  })
  
  clusterManyCode <- callModule(makeCode, "parameters",
                                stringsAsFactors = FALSE)
  
  output$clusterManyCode <- renderText({
    clusterManyCode()
  })
  
  observeEvent(input$run, {
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Computing')  
    renderCE(clusterManyCode())
    
    session$sendCustomMessage(type = 'testmessage',
                              message = 'ClusterMany Computations Completed')  
    })
  
  
  
})





