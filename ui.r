library(shiny)
library(ggvis)
source("global.R")
shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Cluster Many",
                            sidebarLayout(
                              sidebarPanel(
                                h3("Cluster Many Function Inputs"),
                                clusterManyHelpText(),
                                textOutput("clusterManyCode"),
                                actionButton("run", "Run This Code"),
                                dataTableOutput("table")
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Input Arguments for ClusterMany:",
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

