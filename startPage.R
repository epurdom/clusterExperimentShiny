#This .R Script is for the start page. All inoframtion you want to put at the initial pages goes here:
startPageMessage <- function(id, label = "start message") {
    ns <- NS(id)
    #Get back to this
    tagList(
        h2("Welcome to the Shiny App for the clusterExperiment package"),
        fluidRow(
            column(5,
                   h4("Description:"),
                   p("This shiny app implements the clustering and plotting commands found in the clusterExperiment package"),
                   p("The clustering steps of the clusterExperiment package follow the following workflow:"),
                   tags$ul(
                       tags$li("Create initial clusters by varying clustering parameters ('clusterMany')"),
                       tags$li("Find a single clustering by combining these many clusters ('combineMany')"),
                        tags$li("Create a hierarchical ordering of the clusters ('makeDendrogram')"),
                       tags$li("Merge together similar clusters ('mergeClusters')")
                       ),
                   h4("RSEC (Robust Subsampling and Ensemble based Clustering:"),
                   p("RSEC is the name or our clustering procedure for finding homogeneous clusters for single-cell sequencing and other expression data with large numbers of samples. It follows the workflow described above, but making particular choices in the choices of clusters in the 'clusterMany' step.")
            ),
            column(3,
                   h4("Credits and Information"),
                   "This shinyApp was developed by Liam Purvis and Elizabeth Purdom.",
                   "For bugs, support, and other questions related to the shinyApp see its github page:",
                  "www.github.com/epurdom/clusterExpShiny",
                  tags$hr(),
                  "The clusterExperiment package was developed by Elizabeth Purdom and Davide Risso.",
                  "For bugs, support, and other questions for clusterExperiment see its github page:",
                  "www.github.com/epurdom/clusterExperiment"
            )
        ),
        fluidRow(
            column(12,
                   h2("How to use App:"),
                   p("The tabs at the top of this app represent the different steps in the workflow. Normally a user would progressively start from left to right to complete the clustering workflow, as we will describe below"),
                   h4("Getting Started"),
                   p("Users should always first go to the 'Getting Started' tab. Here, users will be prompted to choose their working directory, upload their data, and decide on the file to record the R code that is used in this session."),
                   p("We support uploading the data in a comma or tab deliminated text file. To do so, click on the 'Upload deliminated file'"),
                   p("You can also upload a summarizedExperiment object from a R session. To do so, you must save the object in the 'rds' format, not the standard 'rda' or 'rdata' file format. This can be done with the 'saveRDS' command in R."),
                   p("Finally, you can also upload a clusterExperiment object created from a previous saved run of this shiny app. The 'Save object' tab in this shny app saves the object as a 'rds' format via 'saveRDS'. If you create a clusterExperiment object via command-line R, you should make sure you save it via 'saveRDS' as well."),
                   h4("RSEC"),
                   p("Users wanting to use the RSEC algorithm should go next to the RSEC tab. Here, they will make choices about the parameters for running this algorithms. Once those choices are made, the user will hit 'Run Code' and the RSEC algorithm will run. The resulting output can be save at the 'Save Object' tab."),
                   p("Note that the RSEC tab will go through all of the steps of the cluster workflow described above in one blow. If you wish to go through each step separately, looking at the choices at each step, you should skip the RSEC tab, and instead start at the clusterMany tab. In the clusterMany tab, there is a checkbox that says 'run RSEC'. If you check this box, only the arguments relevant to RSEC will be available to change. After clusterMany, the user then will follow the additional tabs ('combineMany', 'makeDendrogram' and 'mergeClusters') as described below."),
                   h4("Following the clustering workflow (i.e. not RSEC)"),
                   p("Instructions to come"),
                   h4("Making Customized Plots"),
                   p("Instructions to come"),
                   h4("Saving your object"),
                   p("Instructions to come")
            )
        ),
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
        )
    )
}


# makeFile <- function(id, label = "make File") {
#   observeEvent(input$createReproducibleFile, {
#     file.create(input$filePath)
#     
#   })
# }