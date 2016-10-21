#################
# Capture user inputs and make code
# Reactive function which builds the code being run by R:
# Naming convention: for options 'abc', input$aAbc describes a logical as to whether it was added, and input$abc is the actual values that were chosen.
#################

#' @rdname InternalModules
#' @export
makeClusterManyCode <- function(input, output, session, stringsAsFactors, isRSEC=FALSE,countModule) {
    
    clusterManyCode <- reactive({
        clusterManyCode <- paste("")
        #-------
        # Core arguments
        #-------
        if(testArguments(countModule,"isCount") && countModule[["isCount"]]=="TRUE"){
            clusterManyCode<-paste(clusterManyCode,", isCount=",countModule[["isCount"]],"")
        }
        else{
            if(testArguments(countModule,"transFun") && !is.null(countModule[["transFun"]])) clusterManyCode<-paste(clusterManyCode,", transFun=",countModule[["transFun"]])
        }
        clusterManyCode<-combineArgs(input, clusterManyCode,"clusterFunction",isCharacter=TRUE)
        if(!isRSEC){
            clusterManyCode<-combineArgs(input, clusterManyCode,"subsample",isCharacter=FALSE)
            clusterManyCode<-combineArgs(input, clusterManyCode,"sequential",isCharacter=FALSE)
        }
        #-------
        # Dimensionality Reduction
        #-------	
        clusterManyCode<-combineArgs(input, clusterManyCode,"dimReduce",isCharacter=TRUE)
        clusterManyCode<-combineArgs(input, clusterManyCode,"nVarDims",isCharacter=FALSE)
        clusterManyCode<-combineArgs(input, clusterManyCode,"nPCADims",isCharacter=FALSE)
        
        #-------
        # Other Arguments
        #-------
        clusterManyCode<-combineArgs(input, clusterManyCode,"alphas",isCharacter=FALSE)
        clusterManyCode<-combineArgs(input, clusterManyCode,"betas",isCharacter=FALSE)
        clusterManyCode<-combineArgs(input, clusterManyCode,"minSizes",isCharacter=FALSE)
        if(!isRSEC){
            clusterManyCode<-combineArgs(input, clusterManyCode,"ks",isCharacter=FALSE)
            clusterManyCode<-combineArgs(input, clusterManyCode,"findBestK",isCharacter=FALSE)
            clusterManyCode<-combineArgs(input, clusterManyCode,"removeSil",isCharacter=FALSE)
            clusterManyCode<-combineArgs(input, clusterManyCode,"silCutoff",isCharacter=FALSE)
        }
        else clusterManyCode<-combineArgs(input, clusterManyCode,"k0s",isCharacter=FALSE)
        
        # not yet implemented
        #	clusterManyCode<-combineArgs(input, clusterManyCode,"distFunction",isCharacter=TRUE)
        
        #-------
        # utility arguments
        #-------
        clusterManyCode<-combineArgs(input, clusterManyCode,"ncores",isCharacter=FALSE)
        clusterManyCode<-combineArgs(input, clusterManyCode,"random.seed",isCharacter=FALSE)
        
        #-------
        # Specialized options for sequential:
        #-------
        seqArgsCode<-":"
        #if(testArguments(input,"remain.n")) browser()
        seqArgsCode<-combineArgs(input,seqArgsCode,"remain.n",isCharacter=FALSE)
        seqArgsCode<-combineArgs(input,seqArgsCode,"top.can",isCharacter=FALSE)
        seqArgsCode<-combineArgs(input,seqArgsCode,"kmin",isCharacter=FALSE)
        seqArgsCode<-combineArgs(input,seqArgsCode,"kmax",isCharacter=FALSE)
        
        if(seqArgsCode!=":"){#then add it
            seqArgsCode<-gsub(":,","",seqArgsCode)
            seqArgsCode<-paste(", seqArgs=list(",seqArgsCode,")")
            clusterManyCode<-paste0(clusterManyCode,seqArgsCode,collapse="")
        }
        #-------
        # Specialized options for subsampling:
        #-------
        subArgsCode<-":"
        #if(testArguments(input,"resamp.num")) browser()
        subArgsCode<-combineArgs(input,subArgsCode,"resamp.num",isCharacter=FALSE)
        subArgsCode<-combineArgs(input,subArgsCode,"samp.p",isCharacter=FALSE)
        subArgsCode<-combineArgs(input,subArgsCode,"classifyMethod",isCharacter=TRUE)
        
        if(subArgsCode!=":"){#then add it
            subArgsCode<-gsub(":,","",subArgsCode)
            subArgsCode<-paste(", subsampleArgs=list(",subArgsCode,")")
            clusterManyCode<-paste0(clusterManyCode,subArgsCode,collapse="")
        }
        
        #    clusterManyCode <- paste(clusterManyCode, ")", sep = "")
        
        clusterManyCode
    })
    return(clusterManyCode)
}

#' @rdname InternalModules
#' @export
makeMakeDendrogramCode <- function(input, output, session, stringsAsFactors) {
    code <- reactive({
        code <- paste("")
        #if(testArguments(input,"dimReduce")) browser()
        code<-combineArgs(input, code,"dimReduce",isCharacter=TRUE)
        code<-combineArgs(input, code,"ndims",isCharacter=FALSE)
        code<-combineArgs(input, code,"ignoreUnassignedVar",isCharacter=FALSE)
        code<-combineArgs(input,code,"whichClusters",isCharacter=TRUE)
        code    
    })
    return(code)
}

#' @rdname InternalModules
#' @export
makeCombineManyCode <- function(input, output, session, stringsAsFactors) {
    code <- reactive({
        # browser()
        code <- paste("")
        code<-combineArgs(input, code,"proportion",isCharacter=FALSE)
        code<-combineArgs(input, code,"propUnassigned",isCharacter=FALSE)
        code<-combineArgs(input, code,"minSize",isCharacter=FALSE)
        code<-combineArgs(input,code,"clusterLabel",isCharacter=TRUE)
        code<-combineArgs(input,code,"whichClusters",isCharacter=TRUE)
    })
    
    return(code)
}

#make code
#' @rdname InternalModules
#' @export
makeMergeClustersCode <- function(input, output, session, stringsAsFactors) {
    code <- reactive({
        code <- paste("cE <- mergeClusters(cE")
        code<-combineArgs(input, code,"mergeMethod",isCharacter=TRUE)
        code<-combineArgs(input, code,"cutoff",isCharacter=FALSE)
        code<-combineArgs(input, code,"isCount",isCharacter=FALSE)
        code<-combineArgs(input,code,"clusterLabel",isCharacter=TRUE)
        code<-combineArgs(input,code,"whichClusters",isCharacter=TRUE)
        code <- paste(code, ")", sep = "")
    })
    
    return(code)
}