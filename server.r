shinyServer(function(input, output) {
  
  output$dataFileCode <- renderText({
    paste("x = ", input$dataFile[1])
  })
  
  output$dimReduceCode <- renderText({
    paste("dimReduce = '", input$dimReduce,"'")
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
  
  #trying to catch incorrect inputs
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
  
  output$clusterAlgCode <- renderText({
    if (input$clusterAlg == "Sequential Cluster using Cluster Distance")
      paste("seqArgs = list( subsample = ", input$subsampleSQC, ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
            ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
            ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
            ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
            ", alpha = ", input$alphaCD, "))")
    else if (input$clusterAlg == "Sequential Cluster using Cluster Subsample")
      paste("seqArgs = list( subsample = ", input$subsampleSQC, ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
            ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
            ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
            ", subsampleArgs = list( classifyMethods = ", input$classifyMethodSC, ", resamp.numSC  = ", input$resamp.numSC,
            ", samp.p = ", input$samp.pSC, "), clusterArgs = list(", input$clusterArgsSC, "))")
    else if(input$clusterAlg == "Cluster Distance")
      paste(" clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
            ", alpha = ", input$alphaCD, ")")
    else if (input$clusterAlg == "Cluster Subsample")
      paste( "subsampleArgs = list( classifyMethods = ", input$classifyMethodSC, ", resamp.num  = ", input$resamp.numSC,
             ", samp.p = ", input$samp.pSC, "), clusterArgs = list(", input$clusterArgsSC, "))")
      
  })
  
  output$subsampleSQCCode <- renderText({
    paste("subsample = ", input$subsampleSQC)
  })
  
  output$top.canSQCCode <- renderText({
    paste("top.can = ", input$top.canSQC)
  })
  
  output$k0SQCCode <- renderText({
    paste("k0 = ", input$k0SQC)
  })
  
  output$betaSQCCode <- renderText({
    paste("beta = ", input$betaSQC)
  })
  
  output$remain.nSQCCode <- renderText({
    paste("remain.n = ", input$remain.nSQC)
  })
  
  output$k.minSQCCode <- renderText({
    paste("k.min = ", input$k.minSQC)
  })
  
  output$k.maxSQCCode <- renderText({
    paste("k.max = ", input$k.maxSQC)
  })
  
  output$verboseSQCCode <- renderText({
    paste("verbose = ", input$verboseSQC)
  })
  
  output$typeAlgCDCode <- renderText({
    paste("typeAlg = ", input$typeAlgCD)
  })
  
  output$checkArgsCDCode <- renderText({
    paste("checkArgs = ", input$checkArgsCD)
  })
  
  output$alphaCDCode <- renderText({
    paste("alpha = ", input$alphaCD)
  })
  
  output$classifyMethodSCCode <- renderText({
    paste("classifyMethod = ", input$classifyMethodSC)
  })
  
  output$clusterArgsSCCode <- renderText({
    paste("clusterArgs = list(", input$clusterArgsSC, ")")
  })
  
  output$resamp.numSCCode <- renderText({
    paste("resamp.num = ", input$resamp.numSC)
  })
  
  output$samp.pSCCode <- renderText({
    paste("samp.p = ", input$samp.pSC)
  })
  
  output$clusterFunctionSQCCode <- renderText({
    paste("clusterFunction = ", input$clusterFunctionSQC)
  })
  
  output$minSizeCDCode <- renderText({
    paste("MinSize = ", input$minSizeCD)
  })
  
  output$ClusterManyCode <- renderText({
    
    clusterManyCode <- paste("ce <- clusterMany(", input$dataFile[1], ", dimReduce = ", input$dimReduce)
    
    if(input$dimReduce == "PCA")
      clusterManyCode <- paste(clusterManyCode, ", nPCADims = '", input$nPCADims, "'")
    else if(input$dimReduce != "none")
      clusterManyCode <- paste(clusterManyCode, ", nVarDims = ", input$nVarDims)
    
    clusterManyCode <- paste(clusterManyCode, ", ks = c(", min(input$ks), ": ", max(input$ks), ")", 
                             ", clusterFunction = '", input$clusterFunction, "'")
    
    if(input$clusterFunction == "hierarchicalK")
      clusterManyCode <- paste(clusterManyCode, ", findBestK = ", input$findBestK, ", removeSil = ", input$removeSil)
    else if(input$clusterFunction != "pam")
      clusterManyCode <- paste(clusterManyCode, ", alphas = c(", input$alphas, ")")
    
    clusterManyCode <- paste(clusterManyCode, ", sequential = ", input$sequential)
    
    if(input$sequential)
      clusterManyCode <- paste(clusterManyCode, ", betas = ", input$betas )
    
    ##Need distFunction & transformFunction
    
    clusterManyCode <- paste(clusterManyCode, ", subsample = ", input$subsample, ", minSizes = ", input$minSizes, 
                             ", ncores = ", input$ncores, ", random.seed = ", input$random.seed, 
                             ", isCount = ", input$isCount, ", verbose = ", input$verbose, ", run = ", input$run, 
                             ", eraseOld = ", input$eraseOld)
    
    if (input$clusterAlg == "Sequential Cluster using Cluster Distance")
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list( subsample = ", input$subsampleSQC, ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
            ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
            ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
            ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
            ", alpha = ", input$alphaCD, "))")
    else if (input$clusterAlg == "Sequential Cluster using Cluster Subsample")
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list( subsample = ", input$subsampleSQC, ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
            ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
            ", kmax = ", input$k.maxSQC, ", verbose = ", input$verbose, 
            ", subsampleArgs = list( classifyMethods = ", input$classifyMethodSC, ", resamp.numSC  = ", input$resamp.numSC,
            ", samp.p = ", input$samp.pSC, "), clusterArgs = list(", input$clusterArgsSC, "))")
    else if(input$clusterAlg == "Cluster Distance")
      clusterManyCode <- paste(clusterManyCode, ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, ", checkArgs = ", input$checkArgsCD, 
            ", alpha = ", input$alphaCD, ")")
    else if (input$clusterAlg == "Cluster Subsample")
      clusterManyCode <- paste(clusterManyCode, ", subsampleArgs = list( classifyMethods = ", input$classifyMethodSC, ", resamp.numSC  = ", input$resamp.numSC,
             ", samp.p = ", input$samp.pSC, "), clusterArgs = list(", input$clusterArgsSC, "))")
    clusterManyCode <- paste(clusterManyCode, ")")
    
    clusterManyCode
    
  })
  
})





