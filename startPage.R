#This .R Script is for the start page. All inoframtion you want to put at the initial pages goes here:
startPageMessage <- function(id, label = "start message") {
  ns <- NS(id)
  fluidRow(
    column(12,
      h2("Welcome to the Shiny App for Bioconductor's clusterExperiment package"),
      fluidRow(
          column(4,
              h4("Would you like to create a reproducible R script from this work session?")
          ),
          column(4,
              checkboxInput(ns("makeScript"), label = "create script", value = FALSE)
          )
      ),
      conditionalPanel(condition = paste0("input['", ns("makeScript"), "']"),
          fluidRow(
            column(6, 
                   h4("Please enter file path and name (of type .r) in order to create a R file of this session's work:"),
                   textInput(ns("filePath"), label = "eg: 'homeDirectory/subdirectory/filename.r", 
                             value = "~/clusterExperimentShinyAppOutput/clustering.r", width = '100%')
            ),
            column(6, 
                   h4("Please enter any descriptive comments for the beginning of the R file:"),
                   textInput(ns("fileComments"), label = "eg: Name, date, experiment", value = "")
            )
          ),
          actionButton(ns("createReproducibleFile"), label = "Create File")
      ),
      h4("description:"),
      h4("links to github"),
      h4("How to use App: RSEC vs specialized")
    
  )
  )
}


# makeFile <- function(id, label = "make File") {
#   observeEvent(input$createReproducibleFile, {
#     file.create(input$filePath)
#     
#   })
# }