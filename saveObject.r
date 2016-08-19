saveObjectMessage <- function(id, label = "start message") {
  ns <- NS(id)
  tagList(
    h2("Save current clusterExperiment Object:"),
    fluidRow(
        column(6, 
            h4("Please enter file path and name (of type .rds) of internal clusterExperiment object to be saved (via 'saveRDS'):"),
            textInput(ns("saveObjectPath"), label = "eg: 'homeDirectory/subdirectory/objectFileName.rds", 
                #how does this work???
                      #value= makeFileWithDateTag(file="clusterObject_final.rds",wd=input[["fileInput-workingDirectory"]]), width = '100%')
            value = "~/clusterExperimentShinyAppOutput/clusteringObject.rda", width = '100%')
        )
    ),
    actionButton(ns("createObject"), label = "Save Object")
  )
}