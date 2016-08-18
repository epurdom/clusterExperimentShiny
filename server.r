#server.r manages the compu





source("global.R")
#This line allows large file uploads
options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output, session) {

  

  
  #####################################################
  # Begin read file outputs
  #####################################################
  
  #When the create workign directory button is clicked, WD is set and the defalt inputs for the save
  #script and save object widgets are created
  observeEvent(input$createWD, {
    setWD(input[["fileInput-workingDirectory"]])
    output$createScriptInputs <- renderUI({
      textInput("filePath", label = "eg: 'homeDirectory/subdirectory/fileName.r", 
                value = input[["fileInput-workingDirectory"]], width = '100%')
    })
    
    output$createObjectInputs <- renderUI({
      textInput("objectPath", label = "eg: 'homeDirectory/subdirectory/objectName.rda", 
                value = input[["fileInput-workingDirectory"]], width = '50%')
    })
  })
  #Sets filepath and logical make file locally in server.r to be referenced whenever script code is run
  makeFile <- FALSE
  filePath <- NULL
  
  #creates reproducible file if it doens't already exist when clicked
  observeEvent(input$createReproducibleFile, {
    makeFile <<- TRUE
    filePath <<- input$filePath
    if(!file.exists(filePath)) {
      file.create(filePath)
    }
    #initial comments for start of file 
    cat(paste("\n#Beginning of Shiny script session"), file = filePath, append = TRUE)
    
    cat(paste("\n#Date and time: ", date()), file = filePath, append = TRUE)
    
    cat(paste("\n#", input$fileComments), file = filePath, append = TRUE)
  })
  #calling functions from namespaces for uploading files
  rdaFile <- callModule(rdaFile, "fileInput",
                        stringsAsFactors = FALSE)
  
  datafile <- callModule(dataFile, "fileInput",
                         stringsAsFactors = FALSE)
  
  csvAssayCode <- callModule(csvAssayCode, "fileInput", 
                             stringsAsFactors = FALSE)
  
  colDataFile <- callModule(colDataFile, "fileInput",
                            stringsAsFactors = FALSE)
  
  csvColCode <- callModule(csvColCode, "fileInput", 
                             stringsAsFactors = FALSE)
  
  rowDataFile <- callModule(rowDataFile, "fileInput",
                            stringsAsFactors = FALSE)  
  
  csvRowCode <- callModule(csvRowCode, "fileInput", 
                           stringsAsFactors = FALSE)
  
  #This outputs a summary of sE created from the uploaded rda file 
  output$isRda <- renderUI({
    holder <- rdaFile()
    if (is.null(holder))
      return("No data uploaded yet!")
    else {
      #writing code to script
      if(makeFile) {
        cat("\n", 
          "#loading data:",
          "sE <- readRDS('InsertFileName')", 
          sep = "\n",
          file = filePath, append = TRUE)
      }
      sE <<- holder
      #saves object
      if(input$autoCreateObject) {
        saveRDS(sE, input$objectPath)
      }
      
      #Creating which clusters options in various later tabs, only if object is type cluster experiment
      if(class(sE)[1] == "ClusterExperiment") {
        cE <- sE
        #combine many
        output$combineManyWhichClusters <- renderUI({
          multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?",
                               options = unique(clusterTypes(cE)), val = "whichClusters",
                               help = "a numeric or character vector that specifies which clusters 
                               to compare")
        })
        #make dendrogram
        output$makeDendrogramWhichClusters <- renderUI({
          singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", 
                             options = unique(clusterTypes(cE)), val = "whichCluster", 
                             help = "an integer index or character string that identifies which
                             cluster should be used to make the dendrogram. Default is primaryCluster.")
        })
        
        #plot clusters
        output$plotClustersWhichClusters <- renderUI({
          multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", 
                               options = unique(clusterTypes(cE)), val = "whichClusters", 
                               help = "an integer index or character string that identifies which
                               cluster should be used to plotCluster.")
        })
      }
      #the summary that is being returned
      HTML(
        paste(capture.output(show(sE)), collapse = "<br/>")
      )
    }
  })
  
  #creating Summarized Experiment object from uploaded .csv data
  observeEvent(input$makeObject, {
    
    output$isAssay <- renderUI({
      assay <- datafile()
      colData <- colDataFile()
      rowData <- rowDataFile()
      #appending script:
      if(makeFile && !is.null(assay)) {
        cat("\n", 
            "#loading assay data:",
            "\n",
            "assay <- ",
            csvAssayCode(),
            file = filePath, append = TRUE)
      }
      if(makeFile && !is.null(colData)) {
        cat("\n", 
            "#loading column data:",
            "\n",
            "colData <- ",
            csvColCode(),
            file = filePath, append = TRUE)
      }
      if(makeFile && !is.null(rowData)) {
        cat("\n", 
            "#loading row data:",
            "\n",
            "rowData <- ",
            csvRowCode(),
            file = filePath, append = TRUE)
      }
      #if statements to add the correct, non-null data frames to sE
      if(!is.null(assay) && !is.null(colData) && !is.null(rowData)) {
        sE <<- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData),
                                    rowData = data.matrix(rowData))
        if(makeFile) {
          cat("\n", 
              "#creating sE:",
              "sE <- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData),
                                    rowData = data.matrix(rowData))",
              sep = "\n",
              file = filePath, append = TRUE)
        }
      } else if (!is.null(assay) && !is.null(colData) && is.null(rowData)) {
        sE <<- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData))
        if(makeFile) {
          cat("\n", 
              "#creating sE:",
              "sE <- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData))",
              sep = "\n",
              file = filePath, append = TRUE)
        }
      } else if(!is.null(assay) && is.null(colData) && !is.null(rowData)) {
        sE <<- SummarizedExperiment(assays = data.matrix(assay), rowData = data.matrix(rowData))
        if(makeFile) {
          cat("\n", 
              "#creating sE:",
              "sE <- SummarizedExperiment(assays = data.matrix(assay), rowData = data.matrix(rowData))",
              sep = "\n",
              file = filePath, append = TRUE)
        }
      } else if(!is.null(assay) && is.null(colData) && is.null(rowData)) {
        sE <<- SummarizedExperiment(assays = data.matrix(assay))
        if(makeFile) {
          cat("\n", 
              "#creating sE:",
              "sE <- SummarizedExperiment(assays = data.matrix(assay))",
              sep = "\n",
              file = filePath, append = TRUE)
        }
      } else {
        return(HTML(paste("Error, need to upload data file")))
      }
      
      if(input$autoCreateObject) {
        saveRDS(sE, input$objectPath)
      }
      #print(capture.output(show(sE)))
      #return("xxxxxxxxYxxxxxxxxx")
      # if (is.null(assay)) {
      #   return(HTML(paste("No data uploaded yet")))
      # }
      return(HTML(paste(capture.output(show(sE)), collapse = "<br/>")))
    })
    
  })
  
  output$csvAssayContents <- renderTable({
    datafile()[1:4, 1:4]
  })
  
  output$csvColContents <- renderTable({
    colDataFile()[1:4, 1:4]
  })

  output$csvRowContents <- renderTable({
    rowDataFile()[1:4, 1:4]
  })
  
  #inefficient, need insight
  # output$isColData <- renderText({
  #    if (is.null(colDataFile()))
  #      return("")
  #   sE <<- SummarizedExperiment(data.matrix(datafile()), colData = data.matrix(colDataFile()))
  #   return(paste("ColData successfully added, colData dimensions are ", dim(colData(sE))[1], " by ", dim(colData(sE))[2],
  #                "on an sE object with assay of dimensions ",  dim(sE)[1], " by ", dim(sE)[2]))
  #   
  # }) 
  
  # output$isRowData <- renderText({
  #   if (is.null(rowDataFile()))
  #     return("")  
  #   sE <<- SummarizedExperiment(data.matrix(datafile()), colData = data.matrix(colDataFile()), rowData = data.a)
  #   
  #   })
  
  #---------------End read File inputs-----------------
  
  
  #####################################################
  # Begin Save Object Tab
  #####################################################

    observeEvent(input[["saveObject-createObject"]], {
    
    objectPath <- input[["saveObject-filePath"]]
    
    output$saveObjectMessage <- renderText({
      if(dim(cE)[1] == 2 && dim(cE)[2] == 1){
        return("No work completed on uploaded object")
      }
      saveRDS(cE, file = objectPath)
      return(paste("successfully saved internal clusterExperiment object in ", 
                   objectPath, " via function saveRDS()"))
    })

  })
  
  #---------------End save Object tab-----------------
  
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
    

    codeToBeEvaluated <- function() {
      paste("clusterMany(sE, isCount = ", input$isCount, clusterManyCode(),
                               sep = "")
    }
    
    cE <<- eval(parse(text = codeToBeEvaluated()))
    
    if(makeFile) {
      cat("\n", 
        "#Cluster Many tab:",
        codeToBeEvaluated(), 
        "plotClusters(cE)", 
        sep = "\n", file = filePath, append = TRUE)
    }
    
      output$imgCE <- renderPlot({
      # cE is the clusterExperiment object 

      # innerCode <- sub(strsplit(codeToBeEvaluated, ",")[[1]][1],  "clusterMany(sE", 
      #                  codeToBeEvaluated, fixed = TRUE)
      
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE, whichClusters = "clusterMany")
    }, height = max((40/3) * getSEIterations(), 480))
      
      output$combineManyWhichClusters <- renderUI({
        multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                             val = "whichClusters", help = "a numeric or character vector that specifies
                             which clusters to compare")
      })
      
      output$makeDendrogramWhichClusters <- renderUI({
        singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", options = unique(clusterTypes(cE)),
                             val = "whichCluster", help = "an integer index or character string that identifies which
                             cluster should be used to make the dendrogram. Default is primaryCluster.")
      })
      
      output$plotClustersWhichClusters <- renderUI({
        multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                           val = "whichClusters", help = "an integer index or character string that identifies which
                           cluster should be used to plotCluster.")
      })
      
      if(input$autoCreateObject) {
        saveRDS(sE, input$objectPath)
      }
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
  
  combineManyLatterCode <- callModule(makeCombineManyCode, "cMInputs", 
                                stringsAsFactors = FALSE)
  
  combineManyCode <- function(){
    code <- paste("cE <<- combineMany(cE")
    if(input[["cMInputs-aWhichClusters"]])
      code <- paste(code, ", whichClusters = c('", 
                    paste(input[["cMInputs-whichClusters"]], collapse = "','"), "')", sep = "")
    code <- paste(code, combineManyLatterCode(), sep = "")
    return(code)
  }
  
  output$combineManyCode <- renderText({
    combineManyCode()
  })
  # output$combineManyWhichClusters <- renderUI({
  #    multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
  #                         val = "whichClusters", help = "a numeric or character vector that specifies
  #                         which clusters to compare")
  # })
  observeEvent(input$runCombineMany, {
    
    if(makeFile) {
      cat("\n", 
        "#Combine Many Tab",
        combineManyCode(), 
        "#Plotting Clusters:",
        "defaultMar<-par('mar')", 
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)", 
        "par(mar=plotCMar)", 
        "plotClusters(cE, whichClusters = 'clusterMany')", 
        "\n",
        "#Plotting CoClusters:",
        "defaultMar<-par('mar')",
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
        "par(mar=plotCMar)",
        "plotCoClustering(cE)",
        sep = "\n", file = filePath, append = TRUE)
    }
    
    eval(parse(text = combineManyCode()))
    
    output$imgCombineManyPC <- renderPlot({
      # cE is the clusterExperiment object 
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      plotClusters(cE, whichClusters = "clusterMany") #??????????????????? CHECK IF NECESSARY ????????????????
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
      singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", options = unique(clusterTypes(cE)),
                           val = "whichCluster", help = "an integer index or character string that identifies which
                           cluster should be used to make the dendrogram. Default is primaryCluster.")
    })
    
    output$plotClustersWhichClusters <- renderUI({
      multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                         val = "whichClusters", help = "an integer index or character string that identifies which
                         cluster should be used to plotCluster.")
    })
    
    if(input$autoCreateObject) {
      saveRDS(sE, input$objectPath)
    }
    
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
  
  makeDendrogramLatterCode <- callModule(makeMakeDendrogramCode, "mDInputs", 
                                stringsAsFactors = FALSE)
  
  # output$makeDendrogramCode <- renderText({
  #   makeDendrogramCode()
  # })
  
  makeDendrogramCode <- function(){
    code <- paste("cE <<- makeDendrogram(cE")
    if(input[["mDInputs-aWhichCluster"]])
      code <- paste(code, ", whichCluster = '", 
                    paste(input[["mDInputs-whichCluster"]]), "'", sep = "")
    code <- paste(code, makeDendrogramLatterCode(), sep = "")
    return(code)
  }
  
  output$makeDendrogramCode <- renderText({
    makeDendrogramCode()
  })
  
  observeEvent(input$runMakeDendrogram, {
    
    if(makeFile) {
      cat("\n", 
        "#Make Dendrogram Tab:",
        makeDendrogramCode(), 
        "#Plotting Dendrogram",
        "defaultMar<-par('mar')",
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
        "par(mar=plotCMar)",
        "plotDendrogram(cE)",
        "\n",
        "#Plotting Heatmap",
        "defaultMar<-par('mar')",
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
        "par(mar=plotCMar)",
        "plotHeatmap(cE)",
        sep = "\n", file = filePath, append = TRUE)
    }
    
    eval(parse(text = makeDendrogramCode()))
    
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
    
    if(input$autoCreateObject) {
      saveRDS(sE, input$objectPath)
    }
    
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
    
    if(makeFile) {
      cat("\n", 
        "#Merge Clusters Tab:",
        mergeClustersCode(), 
        "#Plotting Clusters after call to merge clusters",
        "defaultMar<-par('mar')",
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
        "par(mar=plotCMar)",
        "plotClusters(cE)",
        "\n",
        "#Plotting Heatmap after call to merge clusters",
        "defaultMar<-par('mar')",
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
        "par(mar=plotCMar)",
        "plotHeatmap(cE)",
        sep = "\n", file = filePath, append = TRUE)
    }
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
    
    output$plotClustersWhichClusters <- renderUI({
      multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                           val = "whichClusters", help = "an integer index or character string that identifies which
                           cluster should be used to plotCluster.")
    })
    
    if(input$autoCreateObject) {
      saveRDS(sE, input$objectPath)
    }
    
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
  # Start Personalized  plotClusters
  #####################################################
  
  
  plotClustersLatterCode <- callModule(makePlotClustersCode, "pCInputs",
                                   stringsAsFactors = FALSE)
  
  plotClustersCode <- function() {
    code <- paste("plotClusters(cE")
    if(input[["pCInputs-aWhichClusters"]]) {
      code <- paste(code, ", whichClusters = c('",
                    paste(input[["pCInputs-whichClusters"]], collapse = "', '"), "')", sep = "")
    }
    code <- paste(code, plotClustersLatterCode(), sep = "")
    return(code)
  }
  
  output$plotClustersCode <- renderText({
    plotClustersCode()
  })
  
  observeEvent(input$runPCCM, {
    
    if(makeFile) {
      cat("\n", 
          "#Specialized call to plotClusters:",
          "#Plotting Clusters",
          "defaultMar<-par('mar')",
          "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
          "par(mar=plotCMar)",
          plotClustersCode(), 
          sep = "\n", file = filePath, append = TRUE)
    }
    
    output$imgPC <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotClustersCode()))
    }, 
    #calculating correct height of image, INCORRECT SYNTAX:
    height = (40/3) * sum(input[["pCInputs-whichClusters"]] == clusterTypes(cE)) )
    
  })
  
  output$downloadSpecializedPlotPCCM <- downloadHandler(
    filename = function(){ paste("specializedPlotFromClusterMany.png")},
    content = function(file){ 
      #ggsave(fileName(), plot = plotClusters(cE, whichClusters = "clusterMany"), )
      png(file, height = (40/3) * sum(input[["pCInputs-whichClusters"]] == clusterTypes(cE)))
      # defaultMar<-par("mar")
      # plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      # par(mar=plotCMar)
      eval(parse(text = plotClustersCode()))
      dev.off()
    }
  )
  
  #####################################################
  # Start Personalized  plotDendrogram
  #####################################################
  plotDendrogramCode <- callModule(makePlotDendrogramCode, "plotDendrogram",
                                   stringsAsFactors = FALSE)
  
  output$plotDendrogramCode <- renderText({
    plotDendrogramCode()
  })
  
  observeEvent(input$runPlotDendrogram, {
    
    if(makeFile) {
      cat("\n", 
          "#Specialized call to plotDendrogram:",
          "#Plotting dendrogram",
          "defaultMar<-par('mar')",
          "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
          "par(mar=plotCMar)",
          plotDendrogramCode(), 
          sep = "\n", file = filePath, append = TRUE)
    }
    
    output$imgSpecializedPlotDendrogram <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotDendrogramCode()))
    })
  })
  
  
  #####################################################
  # Start Personalized  plotHeatmap
  #####################################################
  
  plotHeatmapCode <- callModule(makePlotHeatmapCode, "plotHeatmap",
                                   stringsAsFactors = FALSE)
  
  output$plotHeatmapCode <- renderText({
    plotHeatmapCode()
  })
  
  observeEvent(input$runPlotHeatmap, {
    
    if(makeFile) {
      cat("\n", 
          "#Specialized call to plotDendrogram:",
          "#Plotting dendrogram",
          "defaultMar<-par('mar')",
          "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
          "par(mar=plotCMar)",
          plotHeatmapCode(), 
          sep = "\n", file = filePath, append = TRUE)
    }
    
    output$imgSpecializedPlotHeatmap <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotHeatmapCode()))
    })
  })
  
  
  #####################################################
  # Start Personalized  plotCoClustering
  #####################################################
  
  plotCoClusteringCode <- callModule(makePlotCoClusteringCode, "plotCoClustering",
                                stringsAsFactors = FALSE)
  
  output$plotCoClusteringCode <- renderText({
    plotCoClusteringCode()
  })
  
  observeEvent(input$runPlotCoClustering, {
    
    if(makeFile) {
      cat("\n", 
        "#Specialized call to plotDendrogram:",
        "#Plotting dendrogram",
        "defaultMar<-par('mar')",
        "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
        "par(mar=plotCMar)",
        plotCoClusteringCode(), 
        sep = "\n", file = filePath, append = TRUE)
    }
    
    output$imgSpecializedPlotCoClustering <- renderPlot({
      defaultMar<-par("mar")
      plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
      par(mar=plotCMar)
      eval(parse(text = plotCoClusteringCode()))
    })
  })
  
  #####################################################
  # Start What Clusters
  #####################################################
  
  
  observeEvent(input$showSummmary, {
    output$cESummary <- renderTable({
      if(dim(cE)[1] == 2 && dim(cE)[2] == 1 && dim(sE)[1] == 0 && dim(sE)[2] == 2){
        return("No uploaded object")
      } else if(dim(cE)[1] == 2 && dim(cE)[2] == 1) {
        return(data.frame(c("No work done on object", 
                            "run clusterMany to create a cluster experiment", 
                            "Or upload a cluster experiment object"), fix.empty.names = FALSE))
      } else {
        return(workflowClusterDetails(cE))
      }
      
    })
  })

})





