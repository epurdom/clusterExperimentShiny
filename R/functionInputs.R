#################
##Description####
#This file, contains the inputs for most functions (except clusterMany which is in separate file)
#################


#' @rdname InternalModules
#' @export
RSECInputs<-function(id){
    tagList(
        tabPanel("Main Options",
                 h3("Core imputs for RSEC"),
                 clusterFunctionInputs(id,"clusterInputs",isRSEC=TRUE),
                 singleNumericInput(id, sidelabel="Add Proportion Coherence?", aboveLabel="e.g. 0.5", val="combineProportion", defaultValue=0.5,min = 0, max = 1, step = .01 ,help="The proportion of times that two sets of samples should be together in order to be 
                                 grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)", required = FALSE,checkbox=FALSE,functionName="combineMany"),
                 singleNumericInput(id, sidelabel="Set minimum size for final cluster?", aboveLabel="e.g. 10", val="combineMinSize", help="minimum size required for a set of samples to be considered in a cluster because of 
                                shared clustering; samples in clusters below this size are unassigned", required = FALSE,checkbox=FALSE,functionName="combineMany")
                 #              dendroReduce = "mad", dendroNDims = 1000,
                 #              mergeMethod = "adjP", mergeCutoff = 0.05,
        ),
        tabPanel("Dimensional Reduction",
                 #Allows user to enter all inputs
                 h3("Choose Dimensionality Reduction Options"),
                 dimReduceInput(id, "dim inputs",isRSEC=TRUE)
        ),
        tabPanel("Specialized control",
                 specializedInputs(id, "specialized inputs",isRSEC=TRUE)
        )
    )  
}

#combine many module
#' @rdname InternalModules
#' @export
combineManyInput <- function(id, label = "cMInputs") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    tagList(
        tags$hr(),
        singleNumericInput(id, sidelabel="Add Proportion Coherence?", aboveLabel="e.g. 0.5", val="proportion", defaultValue=0.5,min = 0, max = 1, step = .01 ,help="The proportion of times that two sets of samples should be together in order to be 
                           grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)", required = FALSE,checkbox=FALSE,functionName="combineMany"),
        singleNumericInput(id, sidelabel="Add Proportion Unassigned?", aboveLabel="e.g. 0.5", val="propUnassigned", min = 0, max = 1, step = .01 ,help="samples with greater than this proportion of assignments equal to '-1' are assigned 
                           a '-1' cluster value as a last step (only if proportion < 1)", required = FALSE,checkbox=FALSE,functionName="combineMany"),
        singleNumericInput(id, sidelabel="Set minimum size?", aboveLabel="e.g. 10", val="minSize", help="minimum size required for a set of samples to be considered in a cluster because of 
                           shared clustering; samples in clusters below this size are unassigned", required = FALSE,checkbox=FALSE,functionName="combineMany"),
        singleCharacterInput(id, sidelabel="Give cluster Label?", aboveLabel="e.g. 'myFavorite'", val="clusterLabel", help="gives label to resulting cluster to keep track of it, useful if rerun with different parameters many times", required = FALSE,checkbox=FALSE,functionName="combineMany")
        
        )
}

#make dendrogrm module
#' @rdname InternalModules
#' @export
makeDendrogramInput <- function(id, label = "cMInputs") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        dimReduceInput(id, isRSEC=FALSE,singleChoice=TRUE,functionName="makeDendrogram"),
        conditionalPanel(
            condition = setUpConditionalPanelTest( id, val="dimReduce", allOptions=dimReduceOptions, validOptions=c("mad","var","cv")),
            logicalInput(id,sidelabel="Ignore unassigned samples in dimensionality reduction?",require=TRUE,val="ignoreUnassignedVar",
                         help="logical indicating whether dimensionality reduction via top feature variability (i.e. 'var','cv','mad') should ignore unassigned samples in the primary clustering 
                     for calculation of the top features.",multipleAllowed=FALSE,functionName="makeDendrogram")
        ),
        singleOptionsInput(id,sidelabel="Set handling of unassigned samples?", options=c("outgroup", "cluster"),val="unassignedSamples", help="How to handle the clustering of samples unassigned to a cluster. They can either be a separate outgroup in the dendrogram, of clustered to the closest cluster.",required=FALSE, functionName=functionName)
    )
}

#module for emerge clusters
#' @rdname InternalModules
#' @export
mergeClustersInput <- function(id, label = "cMInputs") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        h3("Inputs for Merge Clusters"),
        tags$hr(),
        singleOptionsInput(id,sidelabel="Set merge method",required=TRUE,val="mergeMethod", options=c("adjP", "locfdr", "MB", "JC"), functionName="mergeClusters",
                           help="method for calculating proportion of non-null that will be used to merge clusters (if 'none', no merging will be done). 'JC' refers to
                       the method of Ji and Cai (2007); 
                       'locfdr' refers to the method of Efron (2004); 'MB' refers to the method of Meinshausen and Buhlmann (2005);
                        'adjP' refers to the proportion 
                       of genes that are found significant based on a FDR adjusted p-values (method
                       'BH') and a cutoff of 0.05."),
        singleNumericInput(id,sidelabel="Set cutoff?",aboveLabel="e.g. 0.1",val="cutoff",defaultValue=0.1,step=0.01,min=0,max=1,functionName="mergeClusters",
                           help="minimimum value required for NOT merging a cluster, i.e. two clusters 
                             with the proportion of DE below cutoff will be merged. Must be a value 
                 between 0, 1, where lower values will make it harder to merge clusters."),
        logicalInput(id,sidelabel="Treat data as counts?",val="isCount",required=TRUE,multipleAllowed=FALSE,functionName="mergeClusters",
                     help="logical as to whether input data is a count matrix. Setting isCount=TRUE also means
                             that the log2(count+0.5) will be used as the transformation and the voom adjustment will be made, and will NOT use the 
                             transformation originally setup in loading the data (e.g. by setting isCount=TRUE when loading data). If FALSE, then transformation will be
                             given to the input and will be used for both internal makeDendrogram and 
                             getBestFeatures calls, with no voom correction."),
        singleCharacterInput(id, sidelabel="Give cluster Label?", aboveLabel="e.g. 'myFavorite'", val="clusterLabel", functionName="mergeClusters",help="gives label to resulting cluster to keep track of it, useful if rerun with different parameters many times")
    )
}