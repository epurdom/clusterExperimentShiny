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

#make code
#' @rdname InternalModules
#' @export
makeMakeDendrogramCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("")
    #if(testArguments(input,"dimReduce")) browser()
    code<-combineArgs(input, code,"dimReduce",isCharacter=TRUE)
    code<-combineArgs(input, code,"ndims",isCharacter=FALSE)
    code<-combineArgs(input, code,"ignoreUnassignedVar",isCharacter=FALSE)
    code    
#     if(input$aDimReduce)
#       code <- paste(code, ", dimReduce = '", input$dimReduce, "'", sep = "")
#     if(input$andims)
#       code <- paste(code, ", ndims = ", input$ndims, sep = "")
#     if(input$aIgnoreUnassignedVar)
#       code <- paste(code, ", ignoreUnassignedVar = ", input$ignoreUnassignedVar, sep = "")
#     code <- paste(code, ")", sep = "")
  })
  return(code)
}

#help text
#' @rdname InternalModules
#' @export
makeDendrogramHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for makeDendrogram()"),
    helpText("helptext here")
  )
}