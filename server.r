source("global.R")
options(shiny.maxRequestSize=30*1024^2)
# sE <- SummarizedExperiment()

shinyServer(function(input, output, session) {
  #sE is the Summarized/Cluster Experiment initially loaded which will remain unaltered
  #sE <- SummarizedExperiment()

  avaliableWhichClusters <- function() {
    return(unique(clusterTypes(cE)))
  }
  
  
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
  
  
  output$isRda <- renderUI({
    holder <- rdaFile()
    if (is.null(holder))
      return("No data uploaded yet!")
    else 
      sE <<- holder
    HTML(
      paste("Summarized experiment object successfully created.", "<br/>", 
            "Summary of object:", "<br/>", paste(capture.output(show(sE)), collapse = "<br/>"))
    )
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
    paste("cE <- clusterMany(sE, isCount = ", input$isCount, clusterManyCode())
  })
  
  getSEIterations <- function(){
    codeToBeEvaluated <- paste("clusterMany(sE, run = FALSE, isCount = ", input$isCount, 
                               clusterManyCode(), sep = "")
    return(nrow(eval(parse(text = codeToBeEvaluated))$paramMatrix))
  }
  
  getCEIterations <- function(){
    codeToBeEvaluated <- paste("clusterMany(cE, run = FALSE, isCount = ", input$isCount, 
                               clusterManyCode(), sep = "")
    return(nrow(eval(parse(text = codeToBeEvaluated))$paramMatrix))
  }
  
  output$numClusterIterations <- renderText({
    codeToBeEvaluated <- paste("clusterMany(sE, run = FALSE, isCount = ", input$isCount, 
                               clusterManyCode(), sep = "")
    paste(getSEIterations(), " cluster iterations given these choices.")
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
      #I would like to know if this works as intentioned.
      plotClusters(cE, whichClusters = "clusterMany")
    }, height = (40/3) * getSEIterations())
      
      output$combineManyWhichClusters <- renderUI({
        multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                             val = "whichClusters", help = "a numeric or character vector that specifies
                             which clusters to compare")
      })
      
      output$makeDendrogramWhichClusters <- renderUI({
        multipleOptionsInput("mDInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                             val = "whichClusters", help = "an integer index or character string that identifies which
                             cluster should be used to make the dendrogram. Default is primaryCluster.")
      })
  })
  
  #This function could certainly be refined
  output$downloadDefaultPlotPCCM <- downloadHandler(
    filename = function(){ paste("DefaultPlotFromClusterMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * getSEIterations(), width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE, whichClusters = "clusterMany")
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
    code <- paste("cE <<- combineMany(cE")
    if(input[["cMInputs-aWhichClusters"]])
      code <- paste(code, ", whichClusters = c('", 
                    paste(input[["cMInputs-whichClusters"]], collapse = "','"), "')", sep = "")
    code <- paste(code, combineManyCode(), sep = "")
      return(code)
  })
  # output$combineManyWhichClusters <- renderUI({
  #    multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
  #                         val = "whichClusters", help = "a numeric or character vector that specifies
  #                         which clusters to compare")
  # })
  observeEvent(input$runCombineMany, {
    
    code <- paste("cE <<- combineMany(cE")
    if(input[["cMInputs-aWhichClusters"]])
      code <- paste(code, ", whichClusters = c('", 
                    paste(input[["cMInputs-whichClusters"]], collapse = "','"), "')", sep = "")
    code <- paste(code, combineManyCode(), sep = "")
    
    eval(parse(text = code))
    
    output$imgCombineManyPC <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE, whichClusters = "clusterMany")
    }, height = (40/3) * getSEIterations())
    
    output$imgCombineManyPCC <- renderPlot({
      # cE is the clusterExperiment object 
      #eval(parse(text = combineManyCode()))
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotCoClustering(cE)
      # library(NMF)
      # mat = matrix(rnorm(1000), ncol = 10, nrow = 100)
      # aheatmap(mat) # Should throw error... but doesnt
    })
    
    output$makeDendrogramWhichClusters <- renderUI({
      multipleOptionsInput("mDInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                           val = "whichClusters", help = "an integer index or character string that identifies which
                           cluster should be used to make the dendrogram. Default is primaryCluster.")
    })
    
  })
  
  output$downloadDefaultPlotPCCombineMany <- downloadHandler(
    filename = function(){ paste("defaultPlotFromCombineMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * getSEIterations(), width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      #Need Help here!
      plotClusters(cE, whichClusters = "clusterMany")
      dev.off()
    }
  )
  
  output$downloadDefaultPlotCoClustersCombineMany <- downloadHandler(
    filename = function(){ paste("DefaultPlotCoClustersCombineMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * getSEIterations(), width = 2*480)
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
  
  # output$makeDendrogramCode <- renderText({
  #   makeDendrogramCode()
  # })
  output$makeDendrogramCode <- renderText({
    code <- paste("cE <<- makeDendrogram(cE")
    if(input[["mDInputs-aWhichClusters"]])
    code <- paste(code, ", whichClusters = c('",
                  paste(input[["mDInputs-whichClusters"]], collapse = "', '"), "')", sep = "")
    code <- paste(code, makeDendrogramCode(), sep = "")
    return(code)
  })
  
  observeEvent(input$runMakeDendrogram, {
    
    code <- paste("cE <<- makeDendrogram(cE")
    if(input[["mDInputs-aWhichClusters"]])
      code <- paste(code, ", whichCluster = c('", 
                    paste(input[["mDInputs-whichClusters"]], collapse = "', '"), "')", sep = "")
    code <- paste(code, makeDendrogramCode(), sep = "")
    eval(parse(text = code))
    
    output$imgPlotDendrogram <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotDendrogram(cE)
    })
    
    output$imgPlotHeatmapMD <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotHeatmap(cE)
    })
    
    #**************************************************
    ##This image is output in Merge Clusters Input Page:
    #**************************************************
    output$imgInitalMergeClusters <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      mergeClusters(cE)
    })
    
  })
  
  output$downloadDefaultPlotPDMD <- downloadHandler(
    filename = function(){ paste("DefaultPlotDendrogramFromMakeDendrogram.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = 480, width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = makeDendrogramCode()))
      plotDendrogram(cE)
      dev.off()
    }
  )

  
  output$downloadDefaultPlotPHMD <- downloadHandler(
    filename = function(){ paste("DefaultPlotHeatmapFromMakeDendrogram.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = 480, width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotHeatmap(cE)
      dev.off()
    }
  )
  
  # End make dendrogram tab
  
  #####################################################
  # Start mergeClusters
  #####################################################
  
  mergeClustersCode <- callModule(makeMergeClustersCode, "mergeCInputs", 
                                   stringsAsFactors = FALSE)
  
  output$mergeClustersCode <- renderText({
    mergeClustersCode()
  })
  
  observeEvent(input$runMergeClusters, {
    
    eval(parse(text = mergeClustersCode()))
    
    output$imgPlotClustersMergeClusters <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE)
    }, height = (40/3) * getCEIterations())
    
    output$imgPlotHeatmapMergeClusters <- renderPlot({
      # cE is the clusterExperiment object
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotHeatmap(cE)
    })
  })
  
  output$downloadDefaultPlotClustersMergeClusters <- downloadHandler(
    filename = function(){ paste("DefaultPlotClustersMergeClusters.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * getCEIterations(), width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE)
      dev.off()
    }
  )
  
  output$downloadDefaultPlotHeatmapMergeClusters <- downloadHandler(
    filename = function(){ paste("DefaultPlotHeatmapMergeClusters.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * getCEIterations(), width = 2*480)
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotHeatmap(cE)
      dev.off()
    }
  )

  #####################################################
  # Start Personalized plots: plotClusters
  #####################################################
  
  
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
  
  #####################################################
  # Start Personalized plots: plotDendrogram
  #####################################################
  plotDendrogramCode <- callModule(makePlotDendrogramCode, "plotDendrogram",
                                   stringsAsFactors = FALSE)
  
  output$plotDendrogramCode <- renderText({
    plotDendrogramCode()
  })
  
  observeEvent(input$runPlotDendrogram, {
    output$imgSpecializedPlotDendrogram <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotDendrogramCode()))
    })
  })
  
})





