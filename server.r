source("global.R")
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {
  #sE is the Summarized/Cluster Experiment initially loaded which will remain unaltered
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
  
  rowDataFile <- callModule(rowDataFile, "fileInput",
                            stringsAsFactors = FALSE)  
  
  
  output$isRda <- renderText({
    holder <- rdaFile()
    if (is.null(holder))
      return("No data uploaded yet!")
    else 
      sE <<- holder
    return(paste("Summarized experiment object successfully created, with dimensions of: ", dim(sE)[1], " by ", dim(sE)[2]))
  })
  
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
  
  # output$isRowData <- renderText({
  #   if (is.null(rowDataFile()))
  #     return("")  
  #   sE <<- SummarizedExperiment(data.matrix(datafile()), colData = data.matrix(colDataFile()), rowData = data.a)
  #   
  #   })
  
  #---------------End read File inputs-----------------
  
  #####################################################
  # Begin Cluster Many Tab
  #####################################################
  
  clusterManyCode <- callModule(makeCode, "parameters",
                                stringsAsFactors = FALSE)
  
  clusterManyStartPageCode <- callModule(makeCECode, "fileInput",
                                         stringsAsFactors = FALSE)
  
  output$clusterManyCode <- renderText({
    paste("cE < clusterMany(sE, isCount = ", input$isCount, clusterManyCode())
  })
  
  getIterations <- function(){
    codeToBeEvaluated <- paste("clusterMany(sE, run = FALSE, isCount = ", input$isCount, 
                               clusterManyCode(), sep = "")
    return(nrow(eval(parse(text = codeToBeEvaluated))$paramMatrix))
  }
  
  output$numClusterIterations <- renderText({
    codeToBeEvaluated <- paste("clusterMany(sE, run = FALSE, isCount = ", input$isCount, 
                               clusterManyCode(), sep = "")
    paste(getIterations(), " cluster iterations given these choices.")
  })
  
  
  observeEvent(input$runCM, {
      output$imgCE <- renderPlot({
      # cE is the clusterExperiment object 
      codeToBeEvaluated <- paste("clusterMany(sE, isCount = ", input$isCount, clusterManyCode(),
                                 sep = "")
      # innerCode <- sub(strsplit(codeToBeEvaluated, ",")[[1]][1],  "clusterMany(sE", 
      #                  codeToBeEvaluated, fixed = TRUE)
      
      cE <<- eval(parse(text = codeToBeEvaluated))
      
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      #I would like to know if this works.
      plotClusters(cE, whichClusters = "clusterMany")
    }, height = (40/3) * getIterations())
  })
  
  plotClustersCMCode <- callModule(makePlotClustersCode, "clusterManyPlotClusters",
                                stringsAsFactors = FALSE)
  
  output$plotClustersCodeCM <- renderText({
    plotClustersCMCode()
  })
  
  observeEvent(input$runPCCM, {
    output$imgPCCM <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotClustersCMCode()))
    })
 })
  
  #This function could certainly be refined
  output$downloadDefaultPlotPCCM <- downloadHandler(
    filename = function(){ paste("DefaultPlotFromClusterMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * getIterations(), width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE, whichClusters = "clusterMany")
      dev.off()
      }
  )
  
  output$downloadSpecializedPlotPCCM <- downloadHandler(
    filename = function(){ paste("specializedPlotFromClusterMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file)
      # defaultMar<-par("mar")
      # plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      # par(mar=plotCMar)
      eval(parse(text = plotClustersCMCode()))
      dev.off()
    }
  )
  
  # End Cluster Many tab
  
  #####################################################
  # Start Combine Many Tab
  #####################################################
  
  combineManyCode <- callModule(makeCombineManyCode, "cMInputs", 
                                stringsAsFactors = FALSE)
  
  output$combineManyCode <- renderText({
    combineManyCode()
  })
  
  observeEvent(input$runCombineMany, {
    output$imgCombineManyPC <- renderPlot({
      # cE is the clusterExperiment object 
      eval(parse(text = combineManyCode()))
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE, whichClusters = "clusterMany")
    })
    output$imgCombineManyPCC <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotCoClustering(cE)
      })
  })
  
  output$downloadDefaultPlotPCCombineMany <- downloadHandler(
    filename = function(){ paste("defaultPlotFromCombineMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      #Need Help here!
      plotClusters(cE, whichClusters = "clusterMany")
      dev.off()
    }
  )
  
  # End Combine Many tab
  
  #####################################################
  # Start make Dendrogram Tab
  #####################################################
  
  makeDendrogramCode <- callModule(makeMakeDendrogramCode, "mDInputs", 
                                stringsAsFactors = FALSE)
  
  output$makeDendrogramCode <- renderText({
    makeDendrogramCode()
  })

  observeEvent(input$runMakeDendrogram, {
    output$imgPlotDendrogram <- renderPlot({
      # cE is the clusterExperiment object 
      eval(parse(text = makeDendrogramCode()))
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      makeDendrogram(cE)
    })
    output$imgPlotHeatmapMD <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotHeatmap(cE)
    })
  })
  
})




