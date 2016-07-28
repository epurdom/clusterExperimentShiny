library(shiny)
library(ggvis)
source("global.R")
shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Start Page"),
                   tabPanel("File Upload",
                             navlistPanel(
                               tabPanel("RDA file input",
                                        rdaFileInput("fileInput", "User rda file"),
                                        textOutput("isRda")),
                               tabPanel("CSV format input",
                                  startPageBasics("fileInput", "User file"),
                                  h3("testing"),
                                  textOutput("isAssay"),
                                  h3("more testing..."),
                                  textOutput("isColData"),
                                  textOutput("testText")
                              )
                              
                            )
                            
                    ),
                    tabPanel("RSEC"),
                   navbarMenu("Cluster Many",
                              tabPanel("Cluster Many Inputs",
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
                                          actionButton("runCM", "Run This Code")
                                          )
                                      ),
                                          navlistPanel(
                                            tabPanel("Dimensional Reduction",
                                                     #Allows user to enter all inputs
                                                     h3("Inputs related to dimensional reduction"),
                                                     dimReduceInput("parameters", "dim inputs")
                                            ),
                                            tabPanel("Cluster Function",
                                                   h3("Inputs related to cluster function"),
                                                   clusterFunctionInputs("parameters", "cluster function inputs")
                                            ),
                                            tabPanel("Subsample/Sequence/Betas",
                                                     h3("Inputs related to Subsample/Sequence/Betas"),
                                                     sSBInputs("parameters", "SSB inputs")
                                            ),
                                            tabPanel("Specialized Options",
                                                     specializedInputs("parameters", "specialized inputs")
                                            )
                                          )
                              #)
                            #)
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
                   ),
                    navbarMenu("Combine Many",
                              tabPanel("Combine Many"),
                              tabPanel("Plot Clusters",
                                       tabsetPanel(
                                         tabPanel("Default Plot"),
                                         tabPanel("Specialized Plotting Options"),
                                         tabPanel("Specialized Plotting Options Output")
                                       )
                               ),
                              tabPanel("PCA Plot")
                   ),
                  navbarMenu("Make Dendrogram",
                             tabPanel("Make Dendrogram"
                             ),
                             tabPanel("Plot Dendrogram",
                                      tabsetPanel(
                                        tabPanel("Default Plot"),
                                        tabPanel("Specialized Plotting Options"),
                                        tabPanel("Specialized Plotting Options Output")
                                      )
                              ),
                             tabPanel("Plot HeatMap",
                                      tabsetPanel(
                                        tabPanel("Default Plot"),
                                        tabPanel("Specialized Plotting Options"),
                                        tabPanel("Specialized Plotting Options Output")
                                      )
                              )
                  ),
                  navbarMenu("Merge Clusters",
                             tabPanel("Merge Clusters"),
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
                             tabPanel("Plot Heatmap",
                                      tabsetPanel(
                                        tabPanel("Default Plot"),
                                        tabPanel("Specialized Plotting Options"),
                                        tabPanel("Specialized Plotting Options Output")
                                      )
                              )
                  )

        )
)

