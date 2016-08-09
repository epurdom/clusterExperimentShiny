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
                                                              plotOutput("imgCE"),
                                                              downloadButton("downloadDefaultPlotPCCM", label = "DownLoad this Plot")                                                       
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
                                           downloadButton("downloadDefaultPlotCoClustersCombineMany", label = "DownLoad this Plot"),
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
                                      plotOutput("imgPlotDendrogram"),
                                      downloadButton("downloadDefaultPlotPDMD", label = "DownLoad this Plot")
                              ),
                             tabPanel("Plot HeatMap",
                                      downloadButton("downloadDefaultPlotPHMD", label = "DownLoad this Plot"),
                                      plotOutput("imgPlotHeatmapMD")
                                      )
                           )
                  ),
                  tabPanel("Merge Clusters",
                           fluidRow(),
                           navlistPanel(
                             
                             tabPanel("Merge Clusters",
                                      textOutput("mergeClustersCode"),
                                      actionButton("runMergeClusters", "Run This Code"),
                                      mergeClustersInput("mergeCInputs", "")
                                      ),
                             tabPanel("Plot Clusters",
                                      downloadButton("downloadDefaultPlotClustersMergeClusters", label = "DownLoad this Plot"),
                                      plotOutput("imgPlotClustersMergeClusters")
                              ),
                             tabPanel("Plot Heatmap",
                                      downloadButton("downloadDefaultPlotHeatmapMergeClusters", label = "DownLoad this Plot"),
                                      plotOutput("imgPlotHeatMapMergeClusters")
                             ),
                             tabPanel("PCA Plot"
                                      
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

