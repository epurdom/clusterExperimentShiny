#################
##Description####
#This file, contains the inputs for most functions (except clusterMany which is in separate file)
#################


#' @rdname InputModules
#' @export
RSECInputs<-function(id){
    tagList(
                 h3("Core imputs for RSEC"),
                 clusterFunctionInputs(id,"clusterInputs",isRSEC=TRUE),
                 singleNumericInput(id, sidelabel="Add Proportion Coherence?", aboveLabel="e.g. 0.5", val="combineProportion", defaultValue=0.5,min = 0, max = 1, step = .01 ,help="The proportion of times that two sets of samples should be together in order to be 
                                 grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)", required = FALSE,checkbox=FALSE,functionName="combineMany"),
                 singleNumericInput(id, sidelabel="Set minimum size combined cluster?", aboveLabel="e.g. 10", val="combineMinSize", help="minimum size required for a set of samples to be considered in a cluster because of 
                                shared clustering; samples in clusters below this size are unassigned", required = FALSE,checkbox=FALSE,functionName="combineMany"),
                 singleOptionsInput(id,sidelabel="Set merge method",required=TRUE,val="mergeMethod", options=c("adjP", "locfdr", "MB", "JC"), functionName="mergeClusters",
                                    help="method for calculating proportion of non-null that will be used to merge clusters (if 'none', no merging will be done). 'JC' refers to
                       the method of Ji and Cai (2007); 
                       'locfdr' refers to the method of Efron (2004); 'MB' refers to the method of Meinshausen and Buhlmann (2005);
                        'adjP' refers to the proportion 
                       of genes that are found significant based on a FDR adjusted p-values (method
                       'BH') and a cutoff of 0.05."),
                 singleNumericInput(id,sidelabel="Set cutoff?",aboveLabel="e.g. 0.1",val="mergeCutoff",defaultValue=0.1,step=0.01,min=0,max=1,functionName="mergeClusters",
                                    help="minimimum value required for NOT merging a cluster, i.e. two clusters 
                             with the proportion of DE below cutoff will be merged. Must be a value 
                 between 0, 1, where lower values will make it harder to merge clusters.")
                 
                 #              dendroReduce = "mad", dendroNDims = 1000,
                 #              mergeMethod = "adjP", mergeCutoff = 0.05,
        
    )  
}

#combine many module
#' @rdname InputModules
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
#' @rdname InputModules
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
#' @rdname InputModules
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


#plot clusters input function
#' @rdname InputModules
#' @export
plotClustersInput <- function(id, label = "plotCluster inputs") {
    ns <- NS(id)
    tagList(
        vectorInput(id,sidelabel="Add columns from colData?",val="sampleData",aboveLabel="e.g. c('Batch','BioCluster')",functionName="plotClusters",
                    help="Refers to a column index or column name in the colData slot of the object. Variables will be plotted at bottom of plotClusters plot"),
        singleCharacterInput(id,sidelabel="Choose color for unassigned samples?",aboveLabel="e.g. 'grey'",val="unassignedColor",defaultValue="white",functionName="plotClusters",
                             help="Unassigned samples (-1) will be given this color"),
        singleCharacterInput(id,sidelabel="Choose color for missing samples?",aboveLabel="e.g. 'grey'",val="missingColor",defaultValue="grey",functionName="plotClusters",
                             help="Samples missing in clustering (-2) will be given this color"),
        tags$hr(),
        logicalInput(id,sidelabel="Reuse colors on each row?",required=TRUE,multipleAllowed=FALSE,defaultValue=FALSE,val="reuseColors",functionName="plotClusters",
            help="Whether each row should consist of the same set of colors. By default 
                     (FALSE) each cluster that the algorithm doesn't identify to the previous
                     rows clusters gets a new color."),
        logicalInput(id,sidelabel="Align clusters to first row?",required=TRUE,multipleAllowed=FALSE,defaultValue=FALSE,functionName="plotClusters",val="matchToTop",help="Logical as to whether all clusters should be aligned to the first row. 
                     By default (FALSE) each cluster is aligned to the ordered clusters of the
                     row above it."),
        logicalInput(id,sidelabel="Restart colors each row?",required=TRUE,multipleAllowed=FALSE,val="startNewColors",defaultValue=FALSE,functionName="plotClusters",help="logical, indicating whether in aligning colors between rows of clusters,
                     should the colors restart at beginning of colPalette as long as colors 
                     are not in immediately proceeding row (some of the colors at the end of 
                     bigPalette are a bit wonky, and so if you have a large clusters matrix, 
                     this can be useful)."),
        tags$hr(),
        logicalInput(id,sidelabel="Add tick marks for samples?",required=TRUE,multipleAllowed=FALSE,val="tick",defaultValue=FALSE,functionName="plotClusters",help="logical, indicating whether in aligning colors between rows of clusters,
                     should the colors restart at beginning of colPalette as long as colors 
                     are not in immediately proceeding row (some of the colors at the end of 
                     bigPalette are a bit wonky, and so if you have a large clusters matrix, 
                     this can be useful)."),
    singleNumericInput(id,sidelabel="Move labels inward?",aboveLabel="e.g. 2",val="axisLine",functionName="plotClusters",
                         help="The number of lines in the axis labels on y-axis should be (passed to
                       line = ... in the axis call)"),
    singleCharacterInput(id,sidelabel="Give y-axis label?",aboveLabel="",val="ylab",functionName="plotClusters",
                         help="Label for y-axis"),
    singleCharacterInput(id,sidelabel="Give x-axis label?",aboveLabel="",val="xlab",functionName="plotClusters",
                         help="Label for x-axis"),
    logicalInput(id,sidelabel="Add box around plot?",val="box",required=TRUE,multipleAllowed=FALSE,defaultValue=FALSE,functionName="plotClusters",help="Logical, whether to draw a box arouns the plot")
    

    #missing: minRequireColor (proportion)
  )
}

#plotCoClustering skeleton
#' @rdname InternalModules
#' @export
plotCoClusteringInput <- function(id, label = "plotCluster inputs") {
    ns <- NS(id)
    tagList(
    )
}

#plot Dendrogram module

#' @rdname InternalModules
#' @export
plotDendrogramInput <- function(id, label = "plotDendrogram inputs") {
    ns <- NS(id)
    tagList(
        tags$hr(),
        fluidRow(
            column(3, checkboxInput(ns("aLeaves"), value = FALSE, label = "Add leaves?")),
            conditionalPanel(condition = paste0("input['", ns("aLeaves"), "']"),
                             column(3, radioButtons(ns("leaves"), choices = c("clusters", "samples"), label = NULL))
            ),
            column(2, checkboxInput(ns("hLeaves"), value = FALSE, label = "Help Text and Instructions")),
            conditionalPanel(condition = paste0("input['", ns("hLeaves"), "']"),
                             column(4, helpText("if 'samples' the dendrogram has one leaf per sample, otherwise it has
                                                one per cluster.")
                             )
                             )
            ),
        
        tags$hr(),
        fluidRow(
            column(3, checkboxInput(ns("aClusterNames"), value = FALSE, label = "Add clusterNames?")),
            conditionalPanel(condition = paste0("input['", ns("aClusterNames"), "']"),
                             column(3, checkboxInput(ns("clusterNames"), value = FALSE, label = "TRUE"))
            ),
            column(2, checkboxInput(ns("hClusterNames"), value = FALSE, label = "Help Text and Instructions")),
            conditionalPanel(condition = paste0("input['", ns("hClusterNames"), "']"),
                             column(4, helpText("logical. If leaves='clusters', then clusters will be identified with 
                             their 'name' value in legend; otherwise the 'clusterIds' value will be
                             used.")
                             )
            )
        ),
        tags$hr(),
        h4("main"),
        helpText("need clarity"),
        tags$hr(),
        h4("sub"),
        helpText("need clarity")
        )
}

#' @rdname InternalModules
#' @export
plotHeatmapInput <- function(id, label = "plotCluster inputs") {
    ns <- NS(id)
    tagList(
    )
}
