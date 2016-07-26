source("global.R")
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  
  sE <- SummarizedExperiment()
  
  datafile <- callModule(dataFile, "fileInput",
                         stringsAsFactors = FALSE)
  
  colDataFile <- callModule(colDataFile, "fileInput",
                            stringsAsFactors = FALSE)
  
  sE <- reactive({
    SummarizedExperiment(data.matrix(datafile()))
  })
  
   # output$testText <- renderText({
   #  text <- reactive({
   #    paste("dim assay: ", dim(sE), ", dim colData: ")#, dim(colData(sE)))
   #  })
   #  text()
   # })
  
  #testing
  output$isAssay <- renderText({
     if (is.null(datafile()))
       return("No data uploaded yet")
    sE <<- SummarizedExperiment(data.matrix(datafile()))
    return(paste("Summarized experiment object successfully created, with dimensions of: ", dim(sE)[1], " by ", dim(sE)[2]))
    
  })
  
  
  #inefficient, need insight
  output$isColData <- renderText({
     if (is.null(datafile()))
       return("")
    sE <<- SummarizedExperiment(data.matrix(datafile()), colData = data.matrix(colDataFile()))
    return(paste("ColData successfully added, colData dimensions are ", dim(colData(sE))[1], " by ", dim(colData(sE))[2],
                 "on an sE object with assay of dimensions ",  dim(sE)[1], " by ", dim(sE)[2]))
    
  })
  
  
  clusterManyCode <- callModule(makeCode, "parameters",
                                stringsAsFactors = FALSE)
  
  clusterManyStartPageCode <- callModule(makeCECode, "fileInput",
                                         stringsAsFactors = FALSE)
  
  output$clusterManyCode <- renderText({
    paste(clusterManyStartPageCode(), clusterManyCode())
  })
  
  
  observeEvent(input$run, {
    output$imgCE <- renderText({
 
      cE <<- renderCE(paste(clusterManyStartPageCode(), clusterManyCode()), cE)
      
      #plotClusters(cE)
      dim(cE)
    })
  })
  

  
  
  
})





