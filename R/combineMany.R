#' @rdname InternalModules
#' @export
combineManyHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Specialized Inputs for combineMany()"),
        helpText("helptext here")
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
    singleNumericInput(id=id,sidelabel="Set Proportion?",aboveLabel="e.g. 0.7", val="proportion", min = 0, max = 1, step = .1, help="The proportion of times that two sets of samples should be together in order to be 
                                 grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)" , functionName="combineMany"),
    singleNumericInput(id=id,sidelabel="Set Proportion Unassigned?",aboveLabel="e.g. 0.7", val="propUnassigned", min = 0, max = 1, step = .1,help="samples with greater than this proportion of assignments equal to '-1' are assigned 
                                   a '-1' cluster value as a last step (only if proportion < 1)" , functionName="combineMany"),
    singleNumericInput(id=id,sidelabel="Set Minimum Size?",aboveLabel="e.g. 10", val="minSize",help="minimum size required for a set of samples to be considered in a cluster because of 
                                 shared clustering, passed to clusterD" , functionName="combineMany"),
    vectorInput(id=id,sidelabel="Give Result a label?",aboveLabel="e.g. My Favorite Cluster", val="clusterLabel",
        help="This allows you to give a label to the result of combine many. This is useful if you will rerun combineMany with different parameters. You do not need to put quotes around it." , functionName="combineMany")

#     fluidRow(
#       column(3, checkboxInput(ns("aProportion"), value = FALSE, label = "Add Proportion?")),
#       conditionalPanel(condition = paste0("input['", ns("aProportion"), "']"),
#           column(3, numericInput(ns("proportion"), value = .5, min = 0, max = 1, step = .001, label = "Proportion")),
#           column(2, checkboxInput(ns("hProportion"), value = FALSE, label = "Help Text and Instructions")),
#           conditionalPanel(condition = paste0("input['", ns("hProportion"), "']"),
#               column(4, helpText()
#               )
#           )
#       )
#     ),
#     tags$hr(),
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
#     tags$hr(),
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
#     tags$hr(),
    
  )
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
    #         if(input[["cMInputs-aWhichClusters"]])
    #             code <- paste(code, ", whichClusters = c('", 
    #                           paste(input[["cMInputs-whichClusters"]], collapse = "','"), "')", sep = "")
    
#     if(input$aProportion)
#       code <- paste(code, ", proportion = ", input$proportion)
#     if(input$aPropUnassigned)
#       code <- paste(code, ", propUnassigned = ", input$propUnassigned)
#     if(input$aMinSize)
#       code <- paste(code, ", minSize = ", input$minSize)
#     
#     code <- paste(code, ")")
  })
  
  return(code)
}

