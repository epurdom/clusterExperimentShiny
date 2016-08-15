saveObjectMessage <- function(id, label = "start message") {
  ns <- NS(id)
  tagList(
    h2("Save current clusterExperiment Object:"),
    fluidRow(
        column(6, 
            h4("Please enter file path and name (of type .rda) of internal clusterExperiment object to be saved:"),
            textInput(ns("filePath"), label = "eg: 'homeDirectory/subdirectory/objectFileName.rda", 
            value = "~/clusterExperimentShinyAppOutput/clusteringObject.rda", width = '100%')
        )
    ),
    actionButton(ns("createObject"), label = "Create Object Copy")
  )
}