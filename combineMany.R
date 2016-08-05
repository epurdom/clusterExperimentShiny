combineManyInput <- function(id, label = "cMInputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h2("Inputs for Combine Many"),
    fluidRow(
      column(3, h4("Which Clusters"),
             helpText("UNFINISHED - I am assuming we are just using the most recent cluster from clusterMany")
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aProportion"), value = FALSE, label = "Add Proportion?")),
      conditionalPanel(condition = paste0("input['", ns("aProportion"), "']"),
          column(3, numericInput(ns("proportion"), value = .5, min = 0, max = 1, step = .001, label = "Proportion")),
          column(2, checkboxInput(ns("hProportion"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hProportion"), "']"),
              column(4, helpText("The proportion of times that two sets of samples should be together in order to be 
                                 grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)")
              )
          )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aPropUnassigned"), value = FALSE, label = "Add Proportion Unassigned?")),
      conditionalPanel(condition = paste0("input['", ns("aPropUnassigned"), "']"),
          column(3, numericInput(ns("propUnassigned"), value = .5, min = 0, max = 1, step = .001, label = "Proportion Unassigned")),
          column(2, checkboxInput(ns("hPropUnassigned"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hPropUnassigned"), "']"),
                column(4, helpText("samples with greater than this proportion of assignments equal to '-1' are assigned 
                                   a '-1' cluster value as a last step (only if proportion < 1)")
                )
          )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aMinSize"), value = FALSE, label = "Add Minimun Size?")),
      conditionalPanel(condition = paste0("input['", ns("aMinSize"), "']"),
          column(3, numericInput(ns("minSize"), value = 5, step = .001, label = "Min Size")),
          column(2, checkboxInput(ns("hMinSize"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hMinSize"), "']"),
              column(4, helpText("minimum size required for a set of samples to be considered in a cluster because of 
                                 shared clustering, passed to clusterD")
              )
          )
        )
      ),
    tags$hr(),
    fluidRow(h4("clusterLabel"), helpText("Need more info"))
    
  )
}


makeCombineManyCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("cE <- combineMany(cE")#, whichClusters = 'clusterMany' ")
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