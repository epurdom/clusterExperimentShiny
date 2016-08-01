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
      column(3, checkboxInput(ns("aClusterFunction"), value = FALSE, label = "Add Cluster Function?")),
      conditionalPanel(condition = paste0("input['", ns("aClusterFunction"), "']"),
          column(3),
          column(2, checkboxInput(ns("hClusterFunction"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hClusterFunction"), "']"),
                column(4, helpText("the clustering to use (passed to clusterD); currently must be of type '01'"))
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
    fluidRow(
      column(3, checkboxInput(ns("aEraseOld"), value = FALSE, label = "Add Erase Old?")),
      conditionalPanel(condition = paste0("input['", ns("aEraseOld"), "']"),
          column(3, checkboxInput(ns("eraseOld"), value = FALSE, label = "Erase Old")),
          column(2, checkboxInput(ns("hEraseOld"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hEraseOld"), "']"),
              column(4, helpText("logical. Only relevant if input x is of class ClusterExperiment. If TRUE, will erase
                                 existing workflow results (clusterMany as well as mergeClusters and combineMany). If 
                                 FALSE, existing workflow results will have '_i' added to the clusterTypes value, where
                                 i is one more than the largest such existing workflow clusterTypes.")
              )
          )
      )
    ),
    tags$hr(),
    fluidRow(h4("clusterLabel?"), helpText("Implement this?"))
    
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
    if(input$aEraseOld)
      code <- paste(code, ", eraseOld = ", input$eraseOld)
    
    code <- paste(code, ")")
  })
  
  return(code)
}