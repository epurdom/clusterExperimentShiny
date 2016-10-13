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
#    fluidRow(
#       column(3, checkboxInput(ns("aProportion"), value = FALSE, label = )),
#       conditionalPanel(condition = paste0("input['", ns("aProportion"), "']"),
#           column(3, numericInput(ns("proportion"), value = .5, , label = "Proportion")),
#           column(2, checkboxInput(ns("hProportion"), value = FALSE, label = "Help Text and Instructions")),
#           conditionalPanel(condition = paste0("input['", ns("hProportion"), "']"),
#               column(4, helpText("The proportion of times that two sets of samples should be together in order to be 
#                                  grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)")
#               )
#           )
#       )
#    ),
#     fluidRow(
#       column(3, checkboxInput(ns("aPropUnassigned"), value = FALSE, label = "Add Proportion Unassigned?")),
#       conditionalPanel(condition = paste0("input['", ns("aPropUnassigned"), "']"),
#           column(3, numericInput(ns("propUnassigned"), value = .5, min = 0, max = 1, step = .001, label = "Proportion Unassigned")),
#           column(2, checkboxInput(ns("hPropUnassigned"), value = FALSE, label = "Help Text and Instructions")),
#           conditionalPanel(condition = paste0("input['", ns("hPropUnassigned"), "']"),
#                 column(4, helpText("samples with greater than this proportion of assignments equal to '-1' are assigned 
#                                    a '-1' cluster value as a last step (only if proportion < 1)")
#                 )
#           )
#       )
#     ),
#    tags$hr(),
#     fluidRow(
#       column(3, checkboxInput(ns("aMinSize"), value = FALSE, label = "Add Minimun Size?")),
#       conditionalPanel(condition = paste0("input['", ns("aMinSize"), "']"),
#           column(3, numericInput(ns("minSize"), value = 5, step = .001, label = "Min Size")),
#           column(2, checkboxInput(ns("hMinSize"), value = FALSE, label = "Help Text and Instructions")),
#           conditionalPanel(condition = paste0("input['", ns("hMinSize"), "']"),
#               column(4, helpText("minimum size required for a set of samples to be considered in a cluster because of 
#                                  shared clustering, passed to clusterD")
#               )
#           )
#         )
#       ),
#    tags$hr(),
#    fluidRow(h4("clusterLabel"), helpText("Need more info"))



#' @rdname InternalModules
#' @export
makeCombineManyCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("")
    if(input$aProportion)
      code <- paste(code, ", proportion = ", input$proportion)
    if(input$aPropUnassigned)
      code <- paste(code, ", propUnassigned = ", input$propUnassigned)
    if(input$aMinSize)
      code <- paste(code, ", minSize = ", input$minSize)
    
    code <- paste(code, ")")
  })
  
  return(code)
}

#' @rdname InternalModules
#' @export
combineManyHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for combineMany()"),
    helpText("helptext here")
  )
}