library(shiny)
library(ggvis)
source("global.R")
shinyUI(navbarPage("Cluster Experiment",
                   tabPanel("Cluster Many",
                            sidebarLayout(
                              sidebarPanel(
                                textOutput("clusterManyCode")
                              ),
                              mainPanel(
                                userFileInput("parameters", "User inputs")                              
                              )
                            )
                   ),
                   tabPanel("Component 2"),
                   navbarMenu("More",
                              tabPanel("Sub-Component A"),
                              tabPanel("Sub-Component B"))
))

