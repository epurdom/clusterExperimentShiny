#This .R Script is for the start page. All inoframtion you want to put at the initial pages goes here:
startPageMessage <- function(id, label = "start message") {
  ns <- NS(id)
  fluidRow(
    column(12,
      h2("Welcome to the Shiny App for Bioconductor's clusterExperiment package"),
      h4("Create reproducible code?"),
      helpText("Would you like to create a reproducible R script from this work session?"),
      checkboxInput(ns("makeScript"), label = NULL, value = FALSE),
      h4("description:"),
      h4("links to github"),
      h4("How to use App: RSEC vs specialized")
    )
  )
}


makeScript <- function(input, output, session, stringsAsFactors) {
  return(input$makeScript)
}