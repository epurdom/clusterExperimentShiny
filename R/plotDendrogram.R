#plot Dendrogram module

#' @rdname InternalModules
#' @export
plotDendrogramInput <- function(id, label = "plotDendrogram inputs") {
  ns <- NS(id)
  tagList(
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aLeaves"), value = FALSE, label = "Add leaves?")),
      conditionalPanel(condition = paste0("input['", ns("aLeaves"), "']"),
          column(3, radioButtons(ns("leaves"), choices = c("clusters", "samples"), label = NULL))
      ),
      column(2, checkboxInput(ns("hLeaves"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hLeaves"), "']"),
          column(4, helpText("if 'samples' the dendrogram has one leaf per sample, otherwise it has
                             one per cluster.")
          )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aClusterNames"), value = FALSE, label = "Add clusterNames?")),
      conditionalPanel(condition = paste0("input['", ns("aClusterNames"), "']"),
          column(3, checkboxInput(ns("clusterNames"), value = FALSE, label = "TRUE"))
      ),
      column(2, checkboxInput(ns("hClusterNames"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hClusterNames"), "']"),
          column(4, helpText("logical. If leaves='clusters', then clusters will be identified with 
                             their 'name' value in legend; otherwise the 'clusterIds' value will be
                             used.")
          )
      )
    ),
    tags$hr(),
    h4("main"),
    helpText("need clarity"),
    tags$hr(),
    h4("sub"),
    helpText("need clarity")
  )
}

#make code
#' @rdname InternalModules
#' @export
makePlotDendrogramCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("plotDendrogram(cE" )
    
    if(input$aLeaves) {
      code <- paste(code, ", leaves = '", input$leaves, "'", sep = "")
    }
    
    if(input$aClusterNames) {
      code <- paste(code, ", clusterNames = ", input$clusterNames, sep = "")
    }
    
    code <- paste(code, ")", sep = "")
  })
  
  return(code)
}

#help text
#' @rdname InternalModules
#' @export
plotDendrogramHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotDendrogram()"),
    helpText("helptext here")
  )
}