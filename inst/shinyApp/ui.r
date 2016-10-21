shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Welcome Page",
                            startPageMessage("startMessage", "")),
                   tabPanel("Getting Started",
                            tabsetPanel(
                                tabPanel( "Setup Working Directory",
                                          setWD("fileInput", ""),
                                          actionButton("createWD", "Choose Working Directory"),
                                          fluidRow(
                                              column(4,
                                                     h4("Would you like to create a reproducible R script from this work session?")
                                              ),
                                              column(4,
                                                     checkboxInput("makeScript", label = "create script", value = FALSE)
                                              )
                                          ),
                                          conditionalPanel(condition = "input.makeScript",
                                                           fluidRow(
                                                               column(6, 
                                                                      h4("Please enter file path and name (of type .r) in order to create a R file of this session's work:"),
                                                                      uiOutput("createScriptInputs")
                                                               ),
                                                               column(6, 
                                                                      h4("Please enter any descriptive comments for the beginning of the R file:"),
                                                                      textInput("fileComments", label = "eg: Name, date, experiment", value = "")
                                                               )
                                                           ),
                                                           actionButton("createReproducibleFile", label = "Create File")
                                          ),
                                          fluidRow(
                                              column(4,
                                                     h4("Would you like to automatically save the internal cluster experiment
                                            object every time it is updated?")
                                              ),
                                              column(4,
                                                     checkboxInput("autoCreateObject", label = "Automatically save object", value = FALSE)
                                              )
                                          ),
                                          conditionalPanel(condition = "input.autoCreateObject",
                                                           h4("Please enter file path and name (with extension .rds, see 'saveRDS') in order to create a continuously updated R object:"),
                                                           uiOutput("createObjectInputs")
                                          )
                                ),
                                
                                tabPanel("Upload Data",
                                         fluidRow(
                                             column(12,p("The following choices regarding transformation of the data (will take effect only when run clusterMany/RSEC)"))
                                         ),
                                         fluidRow(
                                             column(12,countInfo("trans"))
                                         ),
                                         tabsetPanel(
                                             tabPanel("RDS file input",
                                                      rdaFileInput("fileInput", "User rds file"),
                                                      h4("Summary of object uploaded:"),
                                                      uiOutput("isRda")),
                                             tabPanel("CSV format input",
                                                      fluidRow(
                                                          column(8, csvAssay("fileInput", "")),
                                                          column(4, 
                                                                 h5(" First 4 rows and columns of uploaded table:"),
                                                                 tableOutput("csvAssayContents")
                                                          )
                                                      ),
                                                      fluidRow(
                                                          column(8, csvColData("fileInput", "")),
                                                          column(4, 
                                                                 h5(" First 4 rows and columns of uploaded table:"),
                                                                 tableOutput("csvColContents")
                                                          )
                                                      ),
                                                      fluidRow(
                                                          column(8, csvRowData("fileInput", "")),
                                                          column(4, 
                                                                 h5(" First 4 rows and columns of uploaded table:"),
                                                                 tableOutput("csvRowContents")
                                                          )
                                                      ),
                                                      actionButton("makeObject", 
                                                                   "Create Summarized Experiment object from selected data"),
                                                      h5("Summary of summarized experiment created from uploaded data:"),
                                                      #h3(paste(capture.output(show(sE)),collapse="\n")),
                                                      uiOutput("isAssay")
                                             )
                                             
                                         )
                                )
                            )
                   ),
                   tabPanel("RSEC",
#                                                          h3("Core imputs for RSEC")
#                                                 ),
                            fluidRow(
                                column(6,
                                       #Displays basic help text for Shiny App and clusterMany
                                       RSECHelpText()
                                ),
                                column(6,
                                       #textual output of code that is to be run
                                       h3("Code to be run internally:"),
                                       textOutput("RSECCode"),
                                       #Action button that allows one to run above code
                                       actionButton("runRSEC", "Run This Code"),
                                       textOutput("numRSECIterations")
                                )
                            ),
                            navlistPanel(
                                tabPanel("Main Options",
                                         RSECInputs("rsec")
                                ),
                                tabPanel("Dimensionality Reduction",
                                         #Allows user to enter all inputs
                                         h3("Choose Dimensionality Reduction Options"),
                                         dimReduceInput("rsec", "dim inputs",isRSEC=TRUE,sidelabel="Set dimensionality reduction for clustering?"),
                                         dimReduceInput("rsec", isRSEC=TRUE,singleChoice=TRUE,sidelabel="Set dimensionality reduction for making dendrogram?",dimVal="dendroReduce",ndimVal="dendroNDims")
                                         
                                ),
                                tabPanel("Specialized control",
                                         specializedInputs("rsec", "specialized inputs",isRSEC=TRUE)
                                ),
                                tabPanel("Plot Clusters",
                                         tabsetPanel(
                                             tabPanel("Default Plot",
                                                      downloadButton("downloadDefaultPlotPCRSEC", label = "DownLoad this Plot"),
                                                      plotOutput("imgRSEC")                                                     
                                             )
                                         )
                                )
                        )
                   ),
                   tabPanel("Cluster Many",
                            fluidRow(
                                column(6,
                                       #Displays basic help text for Shiny App and clusterMany
                                       clusterManyHelpText()
                                ),
                                column(6,
                                       #textual output of code that is to be run
                                       h3("Code to be run internally:"),
                                       textOutput("clusterManyCode"),
                                       #Action button that allows one to run above code
                                       actionButton("runCM", "Run This Code"),
                                       textOutput("numClusterIterations")
                                )
                            ),
                            navlistPanel(
                                tabPanel("Main Options",
                                         h3("Core imputs for clusterMany"),
                                         sSBInputs("parameters", "SSB inputs")
                                ),
                                tabPanel("Dimensionality Reduction",
                                         #Allows user to enter all inputs
                                         h3("Choose Dimensionality Reduction Options"),
                                         dimReduceInput("parameters", "dim inputs")
                                ),
                                tabPanel("Further clustering options",
                                         h3("Warning!"),
                                         h4("If you change options on the 'Main Options' tab, you should return to this tab to see what options have changed. It is best to complete the 'Main Options' before starting this page"),
                                         clusterFunctionInputs("parameters", "cluster function inputs")
                                ),
                                tabPanel("Specialized control",
                                         specializedInputs("parameters", "specialized inputs")
                                ),
                                tabPanel("Plot Clusters",
                                         tabsetPanel(
                                             tabPanel("Default Plot",
                                                      downloadButton("downloadDefaultPlotPCCM", label = "DownLoad this Plot"),
                                                      plotOutput("imgCE")                                                     
                                             )
                                         )
                                )
                            )
                   ),
                   tabPanel("Combine Many",
                            fluidRow(
                                column(6,
                                       #Displays basic help text for Shiny App and clusterMany
                                       combineManyHelpText("cMInputs")
                                ),
                                column(6,
                                       #textual output of code that is to be run
                                       h3("Code to be run internally:"),
                                       textOutput("combineManyCode"),
                                       #Action button that allows one to run above code
                                       actionButton("runCombineMany", "Run This Code")
                                )
                            ),
                            navlistPanel(
                                tabPanel("Combine Many Inputs",
                                         h2("Inputs for Combine Many"),
                                         uiOutput("combineManyWhichClusters"),
                                         combineManyInput("cMInputs", "")
                                ),
                                tabPanel("Plot Clusters",
                                         downloadButton("downloadDefaultPlotPCCombineMany", label = "DownLoad this Plot"),
                                         plotOutput("imgCombineManyPC")
                                ),
                                tabPanel("Plot CoClusters",
                                         downloadButton("downloadDefaultPlotCoClustersCombineMany", label = "DownLoad this Plot"),
                                         plotOutput("imgCombineManyPCC")
                                )
                            )
                   ),
                   tabPanel("Make Dendrogram",
                            fluidRow(
                                column(6,
                                       #Displays basic help text for Shiny App and clusterMany
                                       makeDendrogramHelpText("cMInputs")
                                ),
                                column(6,
                                       #textual output of code that is to be run
                                       h3("Code to be run internally:"),
                                       textOutput("makeDendrogramCode"),
                                       #Action button that allows one to run above code
                                       actionButton("runMakeDendrogram", "Run This Code")
                                )
                            ),
                            navlistPanel(
                                tabPanel("Make Dendrogram",
                                         h2("Inputs for Make Dendrogram"),
                                         makeDendrogramInput("mDInputs", "")#,
                                         #uiOutput("makeDendrogramWhichClusters")
                                ),
                                tabPanel("Plot Dendrogram",
                                         downloadButton("downloadDefaultPlotPDMD", label = "DownLoad this Plot"),
                                         plotOutput("imgPlotDendrogram")
                                ),
                                tabPanel("Plot HeatMap",
                                         downloadButton("downloadDefaultPlotPHMD", label = "DownLoad this Plot"),
                                         plotOutput("imgPlotHeatmapMD")
                                )
                            )
                   ),
                   tabPanel("Merge Clusters",
                            fluidRow(
                                column(6,
                                       h3("Merge Clusters Function "),
                                       #Displays basic help text for Shiny App and clusterMany
                                       mergeClustersHelpText()
                                ),
                                column(6,
                                       #textual output of code that is to be run
                                       h3("Code to be run internally:"),
                                       #Action button that allows one to run above code
                                       textOutput("mergeClustersCode"),
                                       actionButton("runMergeClusters", "Run This Code")
                                )
                            ),
                            navlistPanel(
                                tabPanel("Initial Dendrogram",
                                         p("Informative Dendrogram for merge cluster inputs:"),
                                         plotOutput("imgInitalMergeClusters")
                                ),
                                tabPanel("Set Parameters",
                                         mergeClustersInput("mergeCInputs", "")
                                ),
                                tabPanel("Plot Clusters",
                                         downloadButton("downloadDefaultPlotClustersMergeClusters", label = "DownLoad this Plot"),
                                         plotOutput("imgPlotClustersMergeClusters")
                                ),
                                tabPanel("Plot Heatmap",
                                         downloadButton("downloadDefaultPlotHeatmapMergeClusters", label = "DownLoad this Plot"),
                                         plotOutput("imgPlotHeatmapMergeClusters")
                                ),
                                tabPanel("PCA Plot",
                                         h3("PCA plot feature in development")
                                )
                            )
                   ),
                   navbarMenu("Personalized Plots",
                              tabPanel("Plot Clusters",
                                       fluidRow(
                                           column(6,
                                                  plotClustersHelpText("pCInputs",
                                                                       "Help Text for plot Clusters, cM")
                                           ),
                                           column(6,
                                                  h3("Code to be Run:"),
                                                  textOutput("plotClustersCode"),
                                                  actionButton("runPCCM", "Run Plot Cluster Code")
                                           )
                                       ),
                                       navlistPanel("Plot Clusters",
                                                    tabPanel("Specialized Inputs",
                                                             h3("Specialized Plot Cluster Inputs"),
                                                             uiOutput("plotClustersWhichClusters"),
                                                             plotClustersInput("pCInputs", 
                                                                               "inputs for plot Clusters, cM")
                                                    ),
                                                    tabPanel("Output Plot",
                                                             downloadButton("downloadSpecializedPlotPCCM", label = "DownLoad this Plot"),
                                                             plotOutput("imgPC")
                                                    )
                                       )
                              ),
                              tabPanel("plot CoClustering",
                                       fluidRow(
                                           column(6,
                                                  plotCoClusteringHelpText("plotCoClustering",
                                                                           "Help Text for plotCoClustering")
                                           ),
                                           column(6,
                                                  h3("Code to be Run:"),
                                                  textOutput("plotCoClusteringCode"),
                                                  actionButton("runPlotCoClustering", "Run Plot CoClustering Code")
                                           )
                                       ),
                                       navlistPanel(
                                           tabPanel("Specialized Inputs"),
                                           tabPanel("Output Plot")
                                       )
                              ),
                              tabPanel("PCA Plot",
                                       navlistPanel(
                                           tabPanel("Specialized Inputs"),
                                           tabPanel("Output Plot")
                                       )
                              ),
                              tabPanel("Plot Dendrogram",
                                       fluidRow(
                                           column(6,
                                                  plotDendrogramHelpText("plotDendrogram",
                                                                         "Help Text for plotDendrogram")
                                           ),
                                           column(6,
                                                  h3("Code to be Run:"),
                                                  textOutput("plotDendrogramCode"),
                                                  actionButton("runPlotDendrogram", "Run Plot Dendrogram Code")
                                           )
                                       ),
                                       navlistPanel("Plot Dendrogram",
                                                    tabPanel("Specialized Inputs",
                                                             plotDendrogramInput("plotDendrogram", 
                                                                                 "inputs for plotDendrogram")
                                                    ),
                                                    tabPanel("Output Plot",
                                                             downloadButton("downloadSpecializedPlotDendrogram",
                                                                            label = "DownLoad this Plot"),
                                                             plotOutput("imgSpecializedPlotDendrogram")
                                                    )
                                       )
                              ),
                              tabPanel("Plot Heatmap",
                                       fluidRow(
                                           column(6,
                                                  plotHeatmapHelpText("plotHeatmap",
                                                                      "Help Text for plotHeatmap")
                                           ),
                                           column(6,
                                                  h3("Code to be Run:"),
                                                  textOutput("plotHeatmapCode"),
                                                  actionButton("runPlotHeatmap", "Run Plot Heatmap Code")
                                           )
                                       ),
                                       navlistPanel(
                                           tabPanel("Specialized Inputs"),
                                           tabPanel("Output Plot")
                                       )
                              )
                   ),
                   tabPanel("Save Object",
                            saveObjectMessage("saveObject", ""),
                            textOutput("saveObjectMessage")
                   ),
                   tabPanel("What clusters",
                            whatClusters("whatClusters", ""),
                            actionButton("showSummmary", "Show Summary"),
                            tableOutput("cESummary")
                   )
)
)

