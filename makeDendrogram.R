makeDendrogramInput <- function(id, label = "cMInputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h2("Inputs for Make Dendrogram"),
    fluidRow(
      column(3, h4("Which Clusters"),
             helpText("UNFINISHED - I am assuming we are just using the most recent cluster from clusterMany")
      )
    ),
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
      column(3, checkboxInput(ns("aNDims"), value = FALSE, label = "Add nDims?")),
      conditionalPanel(condition = paste0("input['", ns("aNDims"), "']"),
          column(3, numericInput(ns("nDims"), label = "nDims", value = 5)),
          column(2, checkboxInput(ns("hNDims"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hNDims"), "']"),
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
    ),
    tags$hr(),
    fluidRow(
      column(3, h4("Cluster"),
             helpText("UNFINISHED")
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, h4("Leaves"),
             helpText("UNFINISHED")
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aClusterNames"), value = FALSE, label = "Add clusterNames?")),
      conditionalPanel(condition = paste0("input['", ns("aClusterNames"), "']"),
          column(3, checkboxInput(ns("clusterNames"), label = "clusterNames", value = FALSE)),
          column(2, checkboxInput(ns("hClusterNames"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hClusterNames"), "']"),
                column(4, helpText("logical. If leaves='clusters', then clusters will be identified with their 'name' 
                                   value in legend; otherwise the 'clusterIds' value will be used.")
                )
          )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, h4("main"),
             helpText("UNFINISHED")
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, h4("sub"),
             helpText("UNFINISHED")
      )
    )
  )
}


makeMakeDendrogramCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("cE <- makeDendrogram(cE ")
    if(input$aDimReduce)
      code <- paste(code, ", dimReduce = ", input$dimReduce)
    if(input$aNDims)
      code <- paste(code, ", nDims = ", input$nDims)
    if(input$aIgnoreUnassignedVar)
      code <- paste(code, ", ignoreUnassignedVar = ", input$ignoreUnassignedVar)
    if(input$aClusterNames)
      code <- paste(code, ", clusterNames = ", input$clusterNames)
    
    code <- paste(code, ")")
  })
  
  return(code)
}