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
                                        textOutput("isRda")),
                               tabPanel("CSV format input",
                                  csvFile("fileInput", "User file"),
                                  h3("testing"),
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
                                            h3("Cluster Many Function Inputs"),
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
                                        tabPanel("Subsample/Sequence/Betas",
                                                 h3("Inputs related to Subsample/Sequence/Betas"),
                                                 sSBInputs("parameters", "SSB inputs")
                                        ),
                                        tabPanel("Dimensional Reduction",
                                                 #Allows user to enter all inputs
                                                 h3("Inputs related to dimensional reduction"),
                                                 dimReduceInput("parameters", "dim inputs")
                                        ),
                                        tabPanel("Cluster Function",
                                               h3("Inputs related to cluster function"),
                                               clusterFunctionInputs("parameters", "cluster function inputs")
                                        ),
                                        tabPanel("Specialized Options",
                                                 specializedInputs("parameters", "specialized inputs")
                                        ),
                                        tabPanel("Plot Clusters",
                                                 tabsetPanel(
                                                     tabPanel("Default Plot",
                                                               downloadButton("downloadDefaultPlotPCCM", label = "DownLoad this Plot"),
                                                               plotOutput("imgCE")
                                                       ),
                                                     tabPanel("Specialized Plotting Options",
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
                                                              plotClusterInput("clusterManyPlotClusters", 
                                                                               "inputs for plot Clusters, cM")
                                                       ),
                                                       tabPanel("Specialized Plotting Options Output",
                                                                downloadButton("downloadSpecializedPlotPCCM", label = "DownLoad this Plot"),
                                                                plotOutput("imgPCCM")
                                                       )
                                                     )
                                                     
                                            )
                                          )
                              #)
                            #)
                              ),
                    tabPanel("Combine Many",
                                fluidRow(),
                                navlistPanel(
                                  tabPanel("Combine Many Inputs",
                                           textOutput("combineManyCode"),
                                           actionButton("runCombineMany", "Run This Code"),
                                           combineManyInput("cMInputs", "")
                                  ),
                                  tabPanel("Plot Clusters",
                                           plotOutput("imgCombineManyPC"),
                                           downloadButton("downloadDefaultPlotPCCombineMany", label = "DownLoad this Plot")
                                   ),
                                  tabPanel("Plot CoClusters",
                                           plotOutput("imgCombineManyPCC")
                                  )
                                )
                   ),
                  tabPanel("Make Dendrogram",
                           fluidRow(),
                           navlistPanel(
                             tabPanel("Make Dendrogram",
                                      textOutput("makeDendrogramCode"),
                                      actionButton("runMakeDendrogram", "Run This Code"),
                                      makeDendrogramInput("mDInputs", "")
                             ),
                             tabPanel("Plot Dendrogram",
                                      plotOutput("imgPlotDendrogram")
                              ),
                             tabPanel("Plot HeatMap",
                                      plotOutput("imgPlotHeatmapMD")
                              )
                           )
                  ),
                  tabPanel("Merge Clusters",
                           fluidRow(),
                           navlistPanel(
                             
                             tabPanel("Merge Clusters",
                                      textOutput("mergeClustersCode"),
                                      mergeClustersInput("mergeCInputs", "")
                                      ),
                             tabPanel("Plot Clusters",
                                      tabsetPanel(
                                        tabPanel("Default Plot"),
                                        tabPanel("Specialized Plotting Options"),
                                        tabPanel("Specialized Plotting Options Output")
                                      )
                              ),
                             tabPanel("Plot Heatmap",
                                      tabsetPanel(
                                        tabPanel("Default Plot"),
                                        tabPanel("Specialized Plotting Options"),
                                        tabPanel("Specialized Plotting Options Output")
                                      )
                             ),
                             tabPanel("PCA Plot",
                                      tabsetPanel(
                                        tabPanel("Default Plot"),
                                        tabPanel("Specialized Plotting Options"),
                                        tabPanel("Specialized Plotting Options Output")
                                      )
                              )
                           )
                  ),
                  tabPanel("Personalized Plots",
                           fluidRow(),
                           navlistPanel(
                             tabPanel("Plot Clusters"),
                             tabPanel("PCA Plot"),
                             tabPanel("Plot Dendrogram"),
                             tabPanel("Plot Heatmap")
                           )
                  )

        )
)

