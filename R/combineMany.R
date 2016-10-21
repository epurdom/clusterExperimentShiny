
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




