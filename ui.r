library(shiny)
library(ggvis)
source("global.R")
shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Start Page",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Introduction")
                              ),
                              mainPanel(
                                startPageBasics("fileInput", "User file"),
                                h3("testing"),
                                textOutput("isAssay"),
                                h3("more testing..."),
                                textOutput("isColData"),
                                textOutput("testText")
                              )
                              
                            )
                            
                    ),
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
                                actionButton("run", "Run This Code")
                                )
                            ),
                              #),
                              #mainPanel(
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
                    tabPanel("Cluster Many Graphical Outputs",
                             textOutput("imgCE")
                    )
                   ),
                  tabPanel("Combine Many"),
                  tabPanel("Merge Clusters"),
                  tabPanel("Make Dendrogram")
                   # navbarMenu("More",
                   #            tabPanel("Sub-Component A"),
                   #            tabPanel("Sub-Component B")
                   # )
        )
)

