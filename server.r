shinyServer(function(input, output) {
  
  output$dataFileCode <- renderText({
    paste("x = ", input$dataFile[1])
  })
  
  output$dimReduceCode <- renderText({
    paste("dimReduce = ", input$dimReduce)
    })
  
  output$nVarDimsCode <- renderText({
    paste("nVarDims = ", input$nVarDims)
  })
  
  output$nPCADimsCode <- renderText({
    paste("nPCADims = ", input$nPCADims)
  })
  
  output$isCountCode <- renderText({
    paste("isCount = ", input$isCount)
  })
  
  output$ksCode <- renderText({
    paste("ks = c(", min(input$ks), ": ", max(input$ks), ")")
  })
  
  output$clusterFunctionCode <- renderText({
    paste("clusterFunction = '", input$clusterFunction, "'")
  })
  
  output$alphasCode <- renderText({
    if( max(as.numeric(unlist(strsplit(input$alphas,",")))) < 1 && min(as.numeric(unlist(strsplit(input$alphas,",")))) > 0)
      paste("alphas = c(", input$alphas, ")")
    else 
      paste("ERROR: please ensure all values are between 0.0 & 1.0")
  })
  
  output$findBestKCode <- renderText({
    paste("findBestK = '", input$findBestK, "'")
  })
  
  output$sequentialCode <- renderText({
    paste("sequential = ", input$sequential)
  })
  
  output$removeSilCode <- renderText({
    paste("removeSil = ", input$removeSil)
  })
  
  output$subsampleCode <- renderText({
    paste("subsample = ", input$subsample)
  })
  
  output$silCutoffCode <- renderText({
    paste("silCutoff = ", input$silCutoff)
  })
  
  output$betasCode <- renderText({
    if( max(as.numeric(unlist(strsplit(input$betas,",")))) < 1 && min(as.numeric(unlist(strsplit(input$betas,",")))) > 0)
      paste("betas = c(", input$betas, ")")
    else 
      paste("ERROR: please ensure all values are between 0.0 & 1.0")
  })
  
  output$minSizesCode <- renderText({
    paste("minSizes = ", input$minSizes)
  })
  
  output$verboseCode <- renderText({
    paste("verbose = ", input$verbose)
  })
  
  output$ncoresCode <- renderText({
    paste("ncores = ", input$ncores)
  })
  
  output$random.seedCode <- renderText({
    paste("random.seed = ", input$random.seed)
  })
  
  output$runCode <- renderText({
    paste("run = ", input$run)
  })
  
  output$eraseOldCode <- renderText({
    paste("eraseOld = ", input$eraseOld)
  })
  
})





