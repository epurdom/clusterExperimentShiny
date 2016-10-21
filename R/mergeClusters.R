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

