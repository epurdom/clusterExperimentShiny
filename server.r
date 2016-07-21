library(shiny)
library(ggvis)
source("global.R")
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  datafile <- callModule(dataFile, "parameters",
                         stringsAsFactors = FALSE)
  #testing
  output$table <- renderDataTable({
    datafile()
  })
  
  clusterManyCode <- callModule(makeCode, "parameters",
                                stringsAsFactors = FALSE)
  
  output$clusterManyCode <- renderText({
    clusterManyCode()
  })
  
  
  observeEvent(input$run, {
    output$strOfCE <- renderText({

      
      # session$sendCustomMessage(type = 'testmessage',
      #                           message = 'Computing')  
      cE <<- renderCE(clusterManyCode(), datafile())
      
      dim(cE)
      
      # session$sendCustomMessage(type = 'testmessage',
      #                           message = 'ClusterMany Computations Completed')  
    })
  })
  

  
  
  
})





