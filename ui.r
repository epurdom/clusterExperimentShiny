library(shiny)
library(ggvis)
source("global.R")
shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Start Page",
                            startPageMessage("startMessage", "")),
                   tabPanel("File Upload",
                            fluidRow(
                              column(3,
                                     h3("Are Data in Counts?"),
                                     helpText("Whether the data are in counts, in which case the default transFun argument is set as log2(x+1). 
                                              This is simply a convenience to the user."),
                                     checkboxInput("isCount", label = NULL, value = FALSE)
                              ),
                              column(3,
                                     h3("transform function")
                              )
                            ),
                             tabsetPanel(
                               tabPanel("RDA file input",
                                        rdaFileInput("fileInput", "User rda file"),
                                        uiOutput("isRda")),
                               tabPanel("CSV format input",
                                  csvFile("fileInput", "User file"),
                                  h3(paste(capture.output(show(sE)))),
                                  textOutput("isAssay"),
                                  h3("more testing..."),
                                  textOutput("isColData"),
                                  textOutput("isRowData")
                              )
                              
                            )
                            
                    ),
                    tabPanel("RSEC"),
                    tabPanel("Cluster Many",
                                      #sidebarLayout(
                                        #sidebarPanel(
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
                                        tabPanel("Dimensional Reduction",
                                                 #Allows user to enter all inputs
                                                 h3("Choose Dimensionality Reduction Options"),
                                                 dimReduceInput("parameters", "dim inputs")
                                        ),
                                        tabPanel("Further clustering options",
                                               h4("Warning!"),
											   h3("If you change options on the 'General Options' tab, you should return to this tab to see what options have changed. It is best to complete the 'General Options' before starting this page"),
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
                              #)
                            #)
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
                                      uiOutput("makeDendrogramWhichClusters"),
                                      makeDendrogramInput("mDInputs", "")
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
                             
                             tabPanel("Merge Clusters Input",
                                      h4("Informative Dendrogram for merge cluster inputs:"),
                                      plotOutput("imgInitalMergeClusters"),
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
                                               plotClustersHelpText("clusterManyPlotClusters",
                                                                    "Help Text for plot Clusters, cM")
                                        ),
                                        column(6,
                                               h3("Code to be Run:"),
                                               textOutput("plotClustersCodeCM"),
                                               actionButton("runPCCM", "Run Plot Cluster Code")
                                        )
                                      ),
                                      navlistPanel("Plot Clusters",
                                                   tabPanel("Specialized Inputs",
                                                            plotClusterInput("clusterManyPlotClusters", 
                                                                             "inputs for plot Clusters, cM")
                                                   ),
                                                   tabPanel("Output Plot",
                                                            downloadButton("downloadSpecializedPlotPCCM", label = "DownLoad this Plot"),
                                                            plotOutput("imgPCCM")
                                                   )
                                      )
                             ),
                             tabPanel("plot CoClustering",
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
                                      navlistPanel(
                                        tabPanel("Specialized Inputs"),
                                        tabPanel("Output Plot")
                                      )
                             )
                           
                  )

        )
)

