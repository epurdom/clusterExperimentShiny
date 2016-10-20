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
        logicalInput(id,"Ignore unassigned samples in dimensionality reduction?",require=TRUE,val="ignoreUnassignedVar",help="logical indicating whether dimensionality reduction via top feature variability 
                                         (i.e. 'var','cv','mad') should ignore unassigned samples in the primary clustering 
                     for calculation of the top features.",functionName="makeDendrogram")
    ),
    singleOptionsInput(id,sidelabel="Set handling of unassigned samples?", options=c("outgroup", "cluster"),val="unassignedSamples", help="How to handle the clustering of samples unassigned to a cluster. They can either be a separate outgroup in the dendrogram, of clustered to the closest cluster.",required=FALSE, functionName=functionName)

    #       singleOptionsInput(id, sidelabel="Set Dimensionailty Reduction?",options=dimReduceOptions,val="dimReduce", help="what type of dimensionality reduction to perform
    #                                  before clustering",functionName="makeDendrogram"),
    #     tags$hr(),
    #     fluidRow(
    #       column(3, checkboxInput(ns("andims"), value = FALSE, label = "Add ndims?")),
    #       conditionalPanel(condition = paste0("input['", ns("andims"), "']"),
    #           column(3, numericInput(ns("ndims"), label = "ndims", value = 5)),
    #           column(2, checkboxInput(ns("hndims"), value = FALSE, label = "Help Text and Instructions")),
    #           conditionalPanel(condition = paste0("input['", ns("hndims"), "']"),
    #               column(4, helpText("integer An integer identifying how many dimensions to reduce
    #                                  to in the reduction specified by dimReduce")
    #               )
    #            )
    #       )
    #     ),
    #     tags$hr(),
    # fluidRow(
#       column(3, checkboxInput(ns("aIgnoreUnassignedVar"), value = FALSE, label = "Add ignoreUnassignedVar?")),
#       conditionalPanel(condition = paste0("input['", ns("aIgnoreUnassignedVar"), "']"),
#             column(3, checkboxInput(ns("ignoreUnassignedVar"), label = "ignoreUnassignedVar", value = FALSE)),
#             column(2, checkboxInput(ns("hIgnoreUnassignedVar"), value = FALSE, label = "Help Text and Instructions")
#             ),
#             conditionalPanel(condition = paste0("input['", ns("hIgnoreUnassignedVar"), "']"),
#                   column(4, helpText("logical indicating whether dimensionality reduction via top feature variability 
#                                      (i.e. 'var','cv','mad') should ignore unassigned samples in the primary clustering 
#                                      for calculation of the top features.")
#                   )
#             )
#       )
#     ),
#     tags$hr(),
#     fluidRow(
#       column(3, h4("UNassignedSamples"),
#              helpText("UNFINISHED")
#       )
#     )
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