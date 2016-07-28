source("global.R")
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  #sE is the SUmmarized experiment initially loaded which will remain unaltered
  sE <- SummarizedExperiment()
  
  #####################################################
  # Begin read file outputs
  #####################################################
  rdaFile <- callModule(rdaFile, "fileInput",
                        stringsAsFactors = FALSE)
  
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
  
  
  output$isRda <- renderText({
    holder <- rdaFile()
    
    if (is.null(holder))
      return("No data uploaded yet!")
    
    else if(class(holder)[1] == "SummarizedExperiment" || class(holder)[1] == "clusterExperiment") {
      sE <<- holder
    }
    return(paste("Summarized experiment object successfully created, with dimensions of: ", dim(sE)[1], " by ", dim(sE)[2]))
  })
  
  #testing
  output$isAssay <- renderText({
    holder <- datafile()
    
    if (is.null(holder))
      return("No data uploaded yet")
    
    else if(class(holder)[1] == "SummarizedExperiment" || class(holder)[1] == "clusterExperiment") {
      sE <<- holder
    }
      
    else {
      sE <<- SummarizedExperiment(data.matrix(datafile()))
    }
    return(paste("Summarized experiment object successfully created, with dimensions of: ", dim(sE)[1], " by ", dim(sE)[2]))
    
  })
  
  
  
  
  #inefficient, need insight
  output$isColData <- renderText({
     if (is.null(colDataFile()))
       return("")
    sE <<- SummarizedExperiment(data.matrix(datafile()), colData = data.matrix(colDataFile()))
    return(paste("ColData successfully added, colData dimensions are ", dim(colData(sE))[1], " by ", dim(colData(sE))[2],
                 "on an sE object with assay of dimensions ",  dim(sE)[1], " by ", dim(sE)[2]))
    
  }) 
  
  #---------------End read File inputs-----------------
  
  #####################################################
  # Begin Cluster Many Tab
  #####################################################
  
  clusterManyCode <- callModule(makeCode, "parameters",
                                stringsAsFactors = FALSE)
  
  clusterManyStartPageCode <- callModule(makeCECode, "fileInput",
                                         stringsAsFactors = FALSE)
  
  output$clusterManyCode <- renderText({
    paste(clusterManyStartPageCode(), clusterManyCode())
  })
  
  
  observeEvent(input$runCM, {
      output$imgCE <- renderPlot({
      # cE is the clusterExperiment object from the clusterMany function,
      # which will remain unaltered so long as clusterMany isn'r run again
      cE <<- renderCE(paste(clusterManyStartPageCode(), clusterManyCode()), datafile())
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE)
    })
  })
  
  plotClustersCMCode <- callModule(makePlotClustersCode, "clusterManyPlotClusters",
                                stringsAsFactors = FALSE)
  
  output$plotClustersCodeCM <- renderText({
    plotClustersCMCode()
  })
  
  observeEvent(input$runPCCM, {
    output$imgPCCM <- renderPlot({
      cE <<- renderCE(paste(clusterManyStartPageCode(), clusterManyCode()), datafile())
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotClustersCMCode()))
      #print("Reached A. B.")
      #print("testing plot specs", plotClustersCMCode())
      #paste("testing plot specs", plotClustersCMCode())
    })
 })
  
  #This function could certainly be refined
  output$downloadDefaultPlotPCCM <- downloadHandler(
    filename = function(){ paste("DefaultPlotFromClusterMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE), )
      png(file)
      # defaultMar<-par("mar")
      # plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      # par(mar=plotCMar)
      plotClusters(cE)
      dev.off()
      }
  )
  
  output$downloadSpecializedPlotPCCM <- downloadHandler(
    filename = function(){ paste("specializedPlotFromClusterMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE), )
      png(file)
      # defaultMar<-par("mar")
      # plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      # par(mar=plotCMar)
      eval(parse(text = plotClustersCMCode()))
      dev.off()
    }
  )
  
  #####################################################
  # End Cluster Many tab
  #####################################################
  
})




