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



