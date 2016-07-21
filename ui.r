library(shiny)
library(ggvis)
source("global.R")
shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Cluster Many",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Cluster Many Function Inputs"),
                                #Displays basic help text for Shiny App and clusterMany
                                clusterManyHelpText(),
                                #textual output of code that is to be run
                                textOutput("clusterManyCode"),
                                #Action button that allows one to run above code
                                actionButton("run", "Run This Code"),
                                #Testing
                                textOutput("strOfCE")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Input Arguments for ClusterMany:",
                                           #Allows user to enter all inputs
                                         userFileInput("parameters", "User inputs")  
                                  ),
                                  tabPanel("Graphical outputs for clusterMany",
                                         h3("This is where the graphical outouts of clusterMany will go")
                                  )
                                )
                              )
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

