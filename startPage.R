#This .R Script is for the start page. All inoframtion you want to put at the initial pages goes here:
startPageMessage <- function(id, label = "start message") {
  ns <- NS(id)
  #Get back to this
  tagList(
      h4("description:"),
      h4("links to github"),
      h4("How to use App: RSEC vs specialized")
  )
}


# makeFile <- function(id, label = "make File") {
#   observeEvent(input$createReproducibleFile, {
#     file.create(input$filePath)
#     
#   })
# }