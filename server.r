shinyServer(function(input, output) {
  
  output$dataFileCode <- renderText({
    paste("x = ", input$dataFile[1])
  })
  
  output$dimReduceCode <- renderText({
    paste("dimReduce = '", input$dimReduce,"'", sep = "")
    })
  
  output$nVarDimsCode <- renderText({
    paste("nVarDims = ", input$nVarDims, sep = "")
  })
  
  output$nPCADimsCode <- renderText({
    paste("nPCADims = ", input$nPCADims, sep = "")
  })
  
  output$isCountCode <- renderText({
    paste("isCount = ", input$isCount, sep = "")
  })
  
  output$ksCode <- renderText({
    paste("ks = c(", min(input$ks), ": ", max(input$ks), ")", sep = "")
  })
  
  output$clusterFunctionCode <- renderText({
    paste("clusterFunction = '", input$clusterFunction, "'", sep = "")
  })
  
  #trying to catch incorrect inputs
  output$alphasCode <- renderText({
    if( max(as.numeric(unlist(strsplit(input$alphas,",")))) < 1 && min(as.numeric(unlist(strsplit(input$alphas,",")))) > 0)
      paste("alphas = c(", input$alphas, ")", sep = "")
    else 
      paste("ERROR: please ensure all values are between 0.0 & 1.0", sep = "")
  })
  
  output$findBestKCode <- renderText({
    paste("findBestK = '", input$findBestK, "'", sep = "")
  })
  
  output$sequentialCode <- renderText({
    paste("sequential = ", input$sequential, sep = "")
  })
  
  output$removeSilCode <- renderText({
    paste("removeSil = ", input$removeSil, sep = "")
  })
  
  output$subsampleCode <- renderText({
    paste("subsample = ", input$subsample, sep = "")
  })
  
  output$silCutoffCode <- renderText({
    paste("silCutoff = ", input$silCutoff, sep = "")
  })
  
  output$betasCode <- renderText({
    if( max(as.numeric(unlist(strsplit(input$betas,",")))) < 1 && min(as.numeric(unlist(strsplit(input$betas,",")))) > 0)
      paste("betas = c(", input$betas, ")", sep = "")
    else 
      paste("ERROR: please ensure all values are between 0.0 & 1.0", sep = "")
  })
  
  # output$minSizesCode <- renderText({
  #   paste("minSizes = ", input$minSizes)
  # })
  
  output$verboseCode <- renderText({
    paste("verbose = ", input$verbose, sep = "")
  })
  
  output$ncoresCode <- renderText({
    paste("ncores = ", input$ncores, sep = "")
  })
  
  output$random.seedCode <- renderText({
    paste("random.seed = ", input$random.seed, sep = "")
  })
  
  output$runCode <- renderText({
    paste("run = ", input$run, sep = "")
  })
  
  output$eraseOldCode <- renderText({
    paste("eraseOld = ", input$eraseOld, sep = "")
  })
  
  output$clusterAlgCode <- renderText({
    if (input$clusterAlg == "Sequential Cluster using Cluster Distance")
      paste("seqArgs = list( subsample = ", input$subsampleSQC, ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
            ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
            ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
            ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
            ", alpha = ", input$alphaCD, "), clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
    else if (input$clusterAlg == "Sequential Cluster using Cluster Subsample")
      paste("seqArgs = list( subsample = ", input$subsampleSQC, ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
            ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
            ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
            ", subsampleArgs = list( classifyMethods = '", input$classifyMethodSC, "'", 
            ", resamp.num  = ", input$resamp.numSC,  ", clusterFunction = '", input$clusterFunctionSC,
            "', samp.p = ", input$samp.pSC, "), clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
    else if(input$clusterAlg == "Cluster Distance")
      paste(" clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
            ", alpha = ", input$alphaCD, "), clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
    else if (input$clusterAlg == "Cluster Subsample")
      paste( "subsampleArgs = list( classifyMethods = ", input$classifyMethodSC, 
             ", clusterFunction = '", input$clusterFunctionSC, "', resamp.num  = ", input$resamp.numSC,
             ", samp.p = ", input$samp.pSC, "), clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
      
  })
  
  output$subsampleSQCCode <- renderText({
    paste("subsample = ", input$subsampleSQC, sep = "")
  })
  
  output$top.canSQCCode <- renderText({
    paste("top.can = ", input$top.canSQC, sep = "")
  })
  
  output$k0SQCCode <- renderText({
    paste("k0 = ", input$k0SQC, sep = "")
  })
  
  output$betaSQCCode <- renderText({
    paste("beta = ", input$betaSQC, sep = "")
  })
  
  output$remain.nSQCCode <- renderText({
    paste("remain.n = ", input$remain.nSQC, sep = "")
  })
  
  output$k.minSQCCode <- renderText({
    paste("k.min = ", input$k.minSQC, sep = "")
  })
  
  output$k.maxSQCCode <- renderText({
    paste("k.max = ", input$k.maxSQC, sep = "")
  })
  
  output$verboseSQCCode <- renderText({
    paste("verbose = ", input$verboseSQC, sep = "")
  })
  
  output$typeAlgCDCode <- renderText({
    paste("typeAlg = ", input$typeAlgCD, sep = "")
  })
  
  output$checkArgsCDCode <- renderText({
    paste("checkArgs = ", input$checkArgsCD, sep = "")
  })
  
  output$alphaCDCode <- renderText({
    paste("alpha = ", input$alphaCD, sep = "")
  })
  
  output$classifyMethodSCCode <- renderText({
    paste("classifyMethod = ", input$classifyMethodSC, sep = "")
  })
  
  output$clusterArgsSCCode <- renderText({
    paste("clusterArgs = list(", input$clusterArgsSC, ")", sep = "")
  })
  
  output$resamp.numSCCode <- renderText({
    paste("resamp.num = ", input$resamp.numSC, sep = "")
  })
  
  output$samp.pSCCode <- renderText({
    paste("samp.p = ", input$samp.pSC, sep = "")
  })
  
  output$clusterFunctionSCCode <- renderText({
    paste("clusterFunction = ", input$clusterFunctionSC, sep = "")
  })
  
  output$minSizeCDCode <- renderText({
    paste("MinSize = ", input$minSizeCD, sep = "")
  })
  
  #Creating the code that will be displayed and run:
  output$ClusterManyCode <- renderText({
    
    clusterManyCode <- paste("ce <- clusterMany(", input$dataFile[1], ", dimReduce = \"", input$dimReduce, "\"", sep = "")
    
    if(input$dimReduce == "PCA")
      clusterManyCode <- paste(clusterManyCode, ", nPCADims = ", input$nPCADims, sep = "")
    else if(input$dimReduce != "none")
      clusterManyCode <- paste(clusterManyCode, ", nVarDims = ", input$nVarDims, sep = "")
    
    clusterManyCode <- paste(clusterManyCode, ", ks = c(", min(input$ks), ": ", max(input$ks), ")", 
                             ", clusterFunction = '", input$clusterFunction, "'", sep = "")
    
    if(input$clusterFunction == "hierarchicalK")
      clusterManyCode <- paste(clusterManyCode, ", findBestK = ", input$findBestK,
                               ", removeSil = ", input$removeSil, sep = "")
    else if(input$clusterFunction != "pam")
      clusterManyCode <- paste(clusterManyCode, ", alphas = c(", input$alphas, ")", sep = "")
    
    clusterManyCode <- paste(clusterManyCode, ", sequential = ", input$sequential, sep = "")
    
    if(input$sequential)
      clusterManyCode <- paste(clusterManyCode, ", betas = ", input$betas, sep = "")
    
    ##Need distFunction & transformFunction
    
    clusterManyCode <- paste(clusterManyCode, ", subsample = ", input$subsample, # ", minSizes = ", input$minSizes, 
                             ", ncores = ", input$ncores, ", random.seed = ", input$random.seed, 
                             ", isCount = ", input$isCount, ", verbose = ", input$verbose, ", run = ", input$run, 
                             ", eraseOld = ", input$eraseOld, sep = "")
    
    if (input$clusterAlg == "Sequential Cluster using Cluster Distance")
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list( subsample = ", input$subsampleSQC, 
                               ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
                               ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
                               ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
                               ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
                               ", alpha = ", input$alphaCD, ", clusterArgs = list(", input$clusterArgsSC, ")))", sep = "")
    else if (input$clusterAlg == "Sequential Cluster using Cluster Subsample")
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list( subsample = ", input$subsampleSQC, 
                               ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
                               ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
                               ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
                               ", subsampleArgs = list( classifyMethods = '", input$classifyMethodSC, 
                               "', resamp.numSC  = ", input$resamp.numSC, ", clusterFunction = '", input$clusterFunctionSC, 
                               "', samp.p = ", input$samp.pSC, ", clusterArgs = list(", input$clusterArgsSC, ")))", sep = "")
    else if(input$clusterAlg == "Cluster Distance")
      clusterManyCode <- paste(clusterManyCode, ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, 
                               ", checkArgs = ", input$checkArgsCD, 
                               ", alpha = ", input$alphaCD, ", clusterArgs = list(", input$clusterArgsSC, ")))", sep = "")
    else if (input$clusterAlg == "Cluster Subsample")
      clusterManyCode <- paste(clusterManyCode, ", subsampleArgs = list( classifyMethods = '", input$classifyMethodSC, 
                               "', resamp.numSC  = ", input$resamp.numSC, ", clusterFunction = '", input$clusterFunctionSC, 
                               "', samp.p = ", input$samp.pSC, ", clusterArgs = list(", input$clusterArgsSC, ")))", sep = "")
#    clusterManyCode <- paste(clusterManyCode, ")")
    
    clusterManyCode
    
  })
  
})





