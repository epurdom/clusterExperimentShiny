#################
##Description####
#This file, clusterManyPage.R, contains the functions, inputs, and server-side computations of the clusterMany tab.
#This userFile input is a very large function that recieves all of the inputs for clusterMany from the user
#################


#################
# First Setup Page
#################
#' @name InternalModules
#' @title Internal modules used by shiny app
#' @export
sSBInputs <- function(id, label = "SSB inputs") {
    ns <- NS(id)
    functionName<-"clusterMany"
    tagList(
        #-------
        #Choose cluster function
        #-------
        multipleOptionsInput(id, sidelabel="Set Cluster Function (required)",options=clusterFunctionChoices,val="clusterFunction", help="Algorithm used for the clustering. ",required=TRUE, functionName=functionName),
        #----
        # Whether Sequential
        #----
        logicalInput(id,sidelabel="Set sequential clustering?", val="sequential", help="Choose whether to use the sequential strategy.",required=FALSE, functionName=functionName), 
        #----
        # Whether Subsample
        #----
        logicalInput(id,sidelabel="Set subsampling?", val="subsample", help="Choose whether to subsample kmeans/pam clustering. If TRUE, the co-occurance between clusterings over subsamples is used as the distance between samples; otherwise the distance function will be determined by argument distFunction (see Clustering tabs).",required=FALSE, functionName=functionName)
    )
}

#################
#Dimension reduction tab
#################
#' @rdname InternalModules
#' @export
dimReduceInput <- function(id, label = "inputs",isRSEC=FALSE){
    # Create a namespace function using the provided id
    functionName<-if(isRSEC) "RSEC" else "clusterMany"
    ns <- NS(id)
    dimReduceOptions<-c("none","PCA", "var","cv", "mad")
    #if(id=="rsec") browser()
    tagList(
        tags$hr(),
        multipleOptionsInput(id,sidelabel="Select Dimensionality Reduction?", options=dimReduceOptions,val="dimReduce", help="What method(s) of dimensionality reduction to perform before clustering.",required=FALSE, functionName=functionName),
        ###Conditional: nPCADims if PCA
        conditionalPanel(
            condition = setUpConditionalPanelTest( id, val="dimReduce", allOptions=dimReduceOptions, validOptions="PCA"),
            tags$hr(),
            vectorInput(id,sidelabel="# PCA dims", aboveLabel="e.g. 5,25,50",val="nPCADims",help="Please enter a list (separated by commas) of the number of PCA dimensions to keep. Used when 'PCA' is identified as choice in dimensionality reduction. If NA is included, then the full dataset will also be included.",required=FALSE,checkbox=TRUE, functionName=functionName)
        ),
        ###Conditional: nVarDims if mad/cv/var
        conditionalPanel(
            condition =  setUpConditionalPanelTest( id, val="dimReduce", allOptions=dimReduceOptions, validOptions=c("mad","var","cv")),
            tags$hr(),
            vectorInput(id,sidelabel="# variable dimensions:", aboveLabel="e.g. 100,500,1000",val="nVarDims", help="A list (separated by commas) of the number of the most variable features to keep. Used when any of 'var', 'cv', or 'mad' is identified as a choice in dimensionality reduction (the same set of values is used for all). If NA is included, then the full dataset will also be included.",required=FALSE,checkbox=TRUE, functionName=functionName)
        )
        
    )
}



#################
#Clustering options tab
#################
#' @rdname InternalModules
#' @export
clusterFunctionChoices<-c("tight", "hierarchical01","hierarchicalK", "pam")

#' @rdname InternalModules
#' @export
clusterFunctionInputs <- function(id, label = "inputs",isRSEC=FALSE) {
    
    functionName<-if(isRSEC) "RSEC" else "clusterMany"
    ns <- NS(id)
    alphaInput<-vectorInput(id,"Set alpha?", "e.g. 0.1,0.2,0.3",val="alphas",  help="List of comma-separated values between 0 and 1 giving values of alpha to be used by 0-1 clustering functions. Determines tightness required in creating clusters from the dissimilarity matrix.",required=FALSE, functionName=functionName)
    betaInput<-vectorInput(id,sidelabel="Set betas parameter?", aboveLabel="e.g. 0.8,0.9", val="betas", help="Comma-separated list of values between 0 and 1. Only used for clustering combinations where sequential=TRUE. Determines the similarity between two clusters required in order to deem the cluster stable as k in subsampling changes", functionName=functionName)
    
    if(!isRSEC){
        kInput<-vectorInput(id,"Set k/k0?", "e.g. 3,5:7",val="ks",  help="When clustering the samples, this argument is interpreted differently depending on other choices for that cluster run. If sequential=TRUE in a clustering, this argument defines the argument k0 of seqCluster. Otherwise, this argument sets the 'k' in the clustering (when using a clustering function that needs 'k'). This argument also sets 'k' for subsampling, if 'subsample=TRUE'. For clusterings where 'findBestK=TRUE', this argument also defines the range of k values to search over.",required=FALSE, functionName=functionName)
        tg<-tagList(
            h4("Options related to all clustering"),
            kInput,
            multipleOptionsInput(id, "Set distance function",val="distFunction",options="Euclidean",required=FALSE,help="This is not yet implemented. Checking the box will have no affect!", functionName=functionName, defaultValue="Euclidean"),
            # #This might need to be down with clusterD by line 27X
            # #Enter Min clustr Sizes, not conditional
            vectorInput(id,"Set minimum cluster size?", "e.g. 3,5,7",val="minSizes",  help="List of comma separated integers defining the minimimum size required for a cluster. Clusters smaller than this are not kept and samples are left unassigned. If sequential chosen, minSize is used for each sequential selection of clusters.",required=FALSE, functionName=functionName),
            #----------
            #If 01 algorithms 
            #----------
            conditionalPanel(condition =  setUpConditionalPanelTest(id,"clusterFunction",allOptions=clusterFunctionChoices, validOptions=c("tight","hierarchical01")),
                             tags$hr(),
                             h4("Options related to clustering functions that take 0-1 input"),
                             alphaInput
            ),
            #----------
            #If K algorithms 
            #----------
            conditionalPanel(condition =		setUpConditionalPanelTest(id,"clusterFunction",allOptions=clusterFunctionChoices, validOptions=c("hierarchicalK","pam")),
                             h4("Options related to clustering functions where K is required"),
                             tags$hr(),
                             logicalInput(id,sidelabel="Find Best K Automatically?", val="findBestK", help="Whether should find best K based on average silhouette width (only used if clusterFunction of type 'K')",required=FALSE, functionName=functionName),
                             logicalInput(id,sidelabel="Remove samples with low silhouette?", val="removeSil", help="logical as to whether remove when silhouette less than 'silCutoff' parameter (only used if clusterFunction of type 'K')",required=FALSE, functionName=functionName),
                             #if removeSil=TRUE, need silcutoff
                             conditionalPanel(condition = setUpConditionalPanelTest(id,"removeSil",allOptions=c("TRUE","FALSE"), validOptions=c("TRUE") ),
                                              vectorInput(id,"Set Silhouette Cutoff?", "e.g. 0,.1,3",val="silCutoff", help="Real-valued numbers in comma separated list giving requirement on minimum silhouette width for sample to be included in cluster (only when removeSil=TRUE).",required=FALSE, functionName=functionName)
                             )
            ),
            conditionalPanel(condition = setUpConditionalPanelTest(id,val="sequential",allOptions=c("TRUE","FALSE"), validOptions="TRUE"),
                             tags$hr(),
                             h4("Options related to sequential clustering"),
                             betaInput
            )
        )}
    else {
        kInput<-vectorInput(id,"Set k0?", "e.g. 3,5:7",val="k0s",  help="When clustering the samples, this argument defines the argument k0 of seqCluster.",required=FALSE, functionName="RSEC")
        tg<-tagList(kInput,alphaInput, betaInput)
    }
    return(tg)
}


#################
# Args options
#################
#' @rdname InternalModules
#' @export
specializedInputs <- function(id, label = "Specializedinputs",isRSEC=FALSE) {
    functionName<-if(isRSEC) "RSEC" else "clusterMany"
    sharedTags<-tagList(
        singleNumericInput(id,sidelabel="Set # cores for parallel processing", aboveLabel="Enter integer values",val="ncores", help="Enter single integer value to indicate the number of cores that should be used. A value greater than 1 will launch parallel processing of the different clustering combinations on different cores using mclapply.",required=FALSE, functionName=functionName),
        singleNumericInput(id,sidelabel="Set random seed for reproducability?", aboveLabel="Enter integer values",val="random.seed", help="Enter a single arbitrary value to set the seed. This seed will be set before every clustering combination, including if the clustering is done on parallel cores.",required=FALSE, functionName=functionName)
    )
    seqTags<-tagList(
        tags$hr(),
        h4("Specialized arguments to control sequential clustering."),
        singleNumericInput(id,sidelabel="Set # samples required to continue sequential?", aboveLabel="(Integer value)",val="remain.n",  help="Should be an integer value. After sequentially finding a cluster, removing samples in the clustering, and iterating, algorithm stops when only this number of samples are remaining",required=FALSE, functionName="seqCluster"),
        singleNumericInput(id,sidelabel="Set # top clusters considered?", aboveLabel="(Integer value)",val="top.can", help="In the sequential process, k is increased in the subsampling, and a stable cluster is identified when two clusters from different k and k+1 are similar; this argument determines how many of the top clusters will be compared in the pairwise for stability (where clusters are ranked by size, unless 'orderBy' is changed). Making this very big will effectively remove this parameter and all pairwise comparisons of all clusters found will be considered. This might result in smaller clusters being found.",required=FALSE, functionName="seqCluster"),
        singleNumericInput(id,sidelabel="Set kmin", aboveLabel="(Integer value)",val="kmin", help="each iteration of sequential detection of clustering will decrease the beginning K of subsampling, but not lower than k.min.",required=FALSE, functionName=functionName),
        singleNumericInput(id,sidelabel="Set kmax", aboveLabel="(Integer value)",val="kmax", help="algorithm will stop if K in iteration is increased beyond this point.",required=FALSE, functionName="seqCluster")
    )
    subTags<-tagList(
        tags$hr(),
        h4("Specialized arguments to control clustering of subsampled samples."),
        singleNumericInput(id,sidelabel="Set number of subsamples to draw", aboveLabel="(Integer value)",val="resamp.num",  help="The number of independent subsamples to draw.",required=FALSE, functionName="subsampleClustering"),
        singleNumericInput(id,sidelabel="Set the proportion of samples to draw", aboveLabel="(value 0-1)",val="samp.p", help="Should be value in (0,1) identifying the the proportion of samples to subsample for each draw.",required=FALSE, functionName="subsampleClustering"),
        singleOptionsInput(id, sidelabel="How compute co-occurance?",options=c("All", "OutOfSample", "InSample"),val="classifyMethod", help="Choose one method for determining which samples should be used in calculating the co-occurance matrix for each subsample draw. 'All'= all samples, 'OutOfSample'= only those not subsampled in the draw, and 'InSample'=only those subsampled in the draw. Note if 'All' isn't chosen it is possible to get NAs in resulting D matrix when there are some samples that were either never in-sample or out-of-sample (particularly a danger if not enough subsamples are taken). This can lead to errors.",required=FALSE, functionName="subsampleClustering")
    )
    if(isRSEC){
        tagList(sharedTags,
                tags$hr(),
                h4("Note:"),
                p("The remaining arguments set here are quite specialized, and most users will not need to set these. Arguments set here will apply globally to all clusterings"),
                seqTags,subTags)
    }
    else{
        tagList(sharedTags,
                tags$hr(),
                h4("Note:"),
                p("The remaining arguments set here are quite specialized, and most users will not need to set these. Arguments set here will apply globally to all clusterings"),
                conditionalPanel(
                    condition = setUpConditionalPanelTest(id, "sequential", allOptions = c("TRUE","FALSE"), validOptions="TRUE"),
                    seqTags
                ),
                conditionalPanel(
                    condition = setUpConditionalPanelTest(id, "subsample", allOptions = c("TRUE","FALSE"), validOptions="TRUE"),
                    subTags)
        )
    }
}
#I may need to store vectors safely by assigning to variables and then inputting them


#################
# Capture user inputs and make code
# Reactive function which builds the code being run by R:
# Naming convention: for options 'abc', input$aAbc describes a logical as to whether it was added, and input$abc is the actual values that were chosen.
#################
#' @rdname InternalModules
#' @export
makeCode <- function(input, output, session, stringsAsFactors, isRSEC=FALSE) {
    
    clusterManyCode <- reactive({
        #browser()
        clusterManyCode <- paste("")
        #-------
        # Core arguments
        #-------
        #if(testArguments(input,"clusterFunction")) browser()
        #browser()
        clusterManyCode<-combineArgs(input, clusterManyCode,"isCount",isCharacter=FALSE)
        #clusterManyCode<-combineArgs(input, clusterManyCode,"transFun",isCharacter=TRUE)
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
clusterManyHelpText <- function() {
    paste(
        # "Summary: The clusterMany function runs many clusterings at once. The user picks options that should be tried, and the clusterMany function will take all combinations of these options, and run the corresponding clustering for each. No clusterings will be calculated until you press 'Run This Code' to the right.",
        "Directions: The user should first set the the core imputs on the starting page. After this, the user can choose to navigate to other tabs to find different options to vary. For all these inputs choosing multiple values means that clusterings with all these values will be tried in combination with all of the other values also already chosen (except some global, esoteric ones under 'Specialized options'). If you do not choose the option, the (single) default will be run. No clusterings will be calculated until you press 'Run This Code' to the right. Under the 'Run This Code' button , you can see how many clusterings will be run based on the options you have chosen so far. "
    )
}


#' @rdname InternalModules
#' @export
getIterations <- function(codeText,isRSEC=FALSE,countIterations=TRUE){
    functionName<-if(isRSEC) "RSEC" else "clusterMany"
    #####
    #make sure updated values
    sE<-get("sE",envir=appGlobal)
    cE<-get("cE",envir=appGlobal)
    filePath<-get("filePath",envir=appGlobal)
    makeFile<-get("makeFile",envir=appGlobal)
    ######
    
    codeToBeNotRun <- paste(functionName,"(sE, run = FALSE ", codeText, ")",sep = "")
    codeToBeRunSE <- paste(functionName,"(sE", codeText, ")",sep = "")
    codeToBeRunCE <- paste(functionName,"(sE", codeText, ")",sep = "")
    nIter<-if(countIterations) nrow(eval(parse(text = codeToBeNotRun))$paramMatrix) else NULL
    return(list(nIter=nIter,fullCodeSE=codeToBeRunSE,fullCodeCE=codeToBeRunCE))
    
}

#     getSEIterations <- function(){
#         #####
#         #make sure updated values
#         sE<-get("sE",envir=appGlobal)
#         cE<-get("cE",envir=appGlobal)
#         filePath<-get("filePath",envir=appGlobal)
#         makeFile<-get("makeFile",envir=appGlobal)
#         ######
#         
#         codeToBeEvaluated <- paste("clusterMany(sE, run = FALSE ", clusterManyCode(), ")",sep = "")
#         return(nrow(eval(parse(text = codeToBeEvaluated))$paramMatrix))
#     }
##EAP: Why are there two of these? CE version is used by plotting, because then cE is defined and needs to be the one used
###However, shouldn't be able to use cE object to get this??? Go back to see if need actually 'getCEIterations' or can get it from the cE object
#     getCEIterations <- function(){
#         #####
#         #make sure updated values
#         sE<-get("sE",envir=appGlobal)
#         cE<-get("cE",envir=appGlobal)
#         filePath<-get("filePath",envir=appGlobal)
#         makeFile<-get("makeFile",envir=appGlobal)
#         ######
#         
#         codeToBeEvaluated <- paste("clusterMany(cE, run = FALSE ",   clusterManyCode(),")", sep = "")
#         return(nrow(eval(parse(text = codeToBeEvaluated))$paramMatrix))
#     }
# ###EAP: What is this code for?? Something to do for when upload existing CE object??? Doesn't seem to be used anywhere...
# makeCECode <- function(input, output, session, stringsAsFactors) {
#     cECode <- reactive({
#         paste(input$rdaFile[1])
#         
#         if(length(input$file) == 0) {
#             cECode <- paste("ce <- clusterMany(", gsub('[.][A-z ]*', '', input$file[1]), 
#                             ", isCount = ", input$isCount, sep = "")
#         } else {
#             cECode <- paste("ce <- clusterMany(", gsub('[.][A-z ]*', '', input$rdaFile[1]), 
#                             ", isCount = ", input$isCount, sep = "")
#         }
#         cECode
#     })
#     
#     return(cECode)
#     
# } # end of makeCECode function
