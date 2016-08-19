#make dendrogrm module
makeDendrogramInput <- function(id, label = "cMInputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aDimReduce"), value = FALSE, label = "Add Dimensionailty Reduction?")),
      conditionalPanel(condition = paste0("input['", ns("aDimReduce"), "']"),
          column(3, radioButtons(ns("dimReduce"), choices = c("none","PCA", "var","cv", "mad"), label = "dimReduce")),
          column(2, checkboxInput(ns("hDimReduce"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hDimReduce"), "']"),
              column(4, helpText("character A character identifying what type of dimensionality reduction to perform
                                 before clustering. Options are 'none','PCA', 'var','cv', and 'mad'. See transform for
                                 more details.")
              )
          )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("andims"), value = FALSE, label = "Add ndims?")),
      conditionalPanel(condition = paste0("input['", ns("andims"), "']"),
          column(3, numericInput(ns("ndims"), label = "ndims", value = 5)),
          column(2, checkboxInput(ns("hndims"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hndims"), "']"),
              column(4, helpText("integer An integer identifying how many dimensions to reduce
                                 to in the reduction specified by dimReduce")
              )
           )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aIgnoreUnassignedVar"), value = FALSE, label = "Add ignoreUnassignedVar?")),
      conditionalPanel(condition = paste0("input['", ns("aIgnoreUnassignedVar"), "']"),
            column(3, checkboxInput(ns("ignoreUnassignedVar"), label = "ignoreUnassignedVar", value = FALSE)),
            column(2, checkboxInput(ns("hIgnoreUnassignedVar"), value = FALSE, label = "Help Text and Instructions")
            ),
            conditionalPanel(condition = paste0("input['", ns("hIgnoreUnassignedVar"), "']"),
                  column(4, helpText("logical indicating whether dimensionality reduction via top feature variability 
                                     (i.e. 'var','cv','mad') should ignore unassigned samples in the primary clustering 
                                     for calculation of the top features.")
                  )
            )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, h4("UNassignedSamples"),
             helpText("UNFINISHED")
      )
    )
  )
}

#make code
makeMakeDendrogramCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("")
    if(input$aDimReduce)
      code <- paste(code, ", dimReduce = '", input$dimReduce, "'", sep = "")
    if(input$andims)
      code <- paste(code, ", ndims = ", input$ndims, sep = "")
    if(input$aIgnoreUnassignedVar)
      code <- paste(code, ", ignoreUnassignedVar = ", input$ignoreUnassignedVar, sep = "")
    code <- paste(code, ")", sep = "")
  })
  return(code)
}

#help text
makeDendrogramHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for makeDendrogram()"),
    helpText("helptext here")
  )
}