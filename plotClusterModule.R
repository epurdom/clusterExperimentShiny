



plotClusterInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, 
             h3("Which Clusters"),
             helpText("waiting on discussion with Elizabeth")
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aSampleData"), value = FALSE, label = "Add sampleData?")),
      conditionalPanel(condition = paste0("input['", ns("aSampleData"), "']"),
          column(3, textInput(ns("sampleData"), label = NULL, value = "NULL"))
      ),
      column(2, checkboxInput(ns("hSampleData"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hSampleData"), "']"),
          column(4, helpText("UNFINISHED-LETS TALK ABOUT THIS If clusters is a matrix, sampleData gives a matrix of additional 
                             cluster/sampleData on the samples to be plotted with the clusterings 
                             given in clusters. Values in sampleData will be added to the end (bottom)
                             of the plot. If clusters is a ClusterExperiment object, sampleData must 
                             be either an index or a character vector that references a column or 
                             column name, respectively, of the colData slot of the ClusterExperiment 
                             object. Please enter either a sequence of numbers separated by commas, 
                             or leave as 'NULL' if you wish for nothing to be passed to this argument.
                             UNFINISHED (NULL is inelegant)"))
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aReuseColors"), value = FALSE, label = "Add reuseColors?")),
      conditionalPanel(condition = paste0("input['", ns("aReuseColors"), "']"),
          column(3, checkboxInput(ns("reuseColors"), value = FALSE, label = "TRUE"))
      ),
      column(2, checkboxInput(ns("hReuseColors"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hReuseColors"), "']"),
          column(4, helpText("Whether each row should consist of the same set of colors. By default 
                             (FALSE) each cluster that the algorithm doesn't identify to the previous
                             rows clusters gets a new color.")
          )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aMatchToTop"), value = FALSE, label = "Add matchToTop?")),
      conditionalPanel(condition = paste0("input['", ns("aMatchToTop"), "']"),
          column(3, checkboxInput(ns("matchToTop"), value = FALSE, label = "TRUE"))
      ),
      column(2, checkboxInput(ns("hMatchToTop"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hMatchToTop"), "']"),
          column(4, helpText("Logical as to whether all clusters should be aligned to the first row. 
                             By default (FALSE) each cluster is aligned to the ordered clusters of the
                             row above it.")
          )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aUnassignedColor"), value = FALSE, label = "Add unassignedColor?")),
      conditionalPanel(condition = paste0("input['", ns("aUnassignedColor"), "']"),
          column(3, radioButtons(ns("unassignedColor"), choices = c("white", "black", "grey"), 
                                 label = NULL, selected = "white")
          )
      ),
      column(2, checkboxInput(ns("hUnassignedColor"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hUnassignedColor"), "']"),
          column(4, helpText("UNFINISHED: What form is color supposed to be in? Default is white? If 
                             “-1” in clusters, will be given this color (meant for samples not 
                             assigned to cluster)")
          )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aMissingColor"), value = FALSE, label = "Add missingColor?")),
      conditionalPanel(condition = paste0("input['", ns("aMissingColor"), "']"),
          column(3, radioButtons(ns("missingColor"), choices = c("white", "black", "grey"), 
                                 label = NULL, selected = "white")
          )
      ),
      column(2, checkboxInput(ns("hMissingColor"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hMissingColor"), "']"),
          column(4, helpText("UNFINISHED: What form is color supposed to be in? Default? If “-1” in
                             clusters, will be given this color (meant for samples not assigned to
                             cluster).")
          )
      )
    ),

    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aMinRequireColor"), value = FALSE, label = "Add minRequireColor?")),
      conditionalPanel(condition = paste0("input['", ns("aMinRequireColor"), "']"),
                       column(3, numericInput(ns("minRequireColor"), label = NULL, value = 100))
      ),
      column(2, checkboxInput(ns("hMinRequireColor"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hMinRequireColor"), "']"),
          column(4, helpText("UNFINISHED- proportion or percent? In aligning colors between rows of 
                             clusters, require this percent overlap.")
          )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aStartNewColors"), value = FALSE, label = "Add startNewColors?")),
      conditionalPanel(condition = paste0("input['", ns("aStartNewColors"), "']"),
          column(3, checkboxInput(ns("startNewColors"), value = FALSE, label = "TRUE"))
      ),
      column(2, checkboxInput(ns("hStartNewColors"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hStartNewColors"), "']"),
          column(4, helpText("logical, indicating whether in aligning colors between rows of clusters,
                             should the colors restart at beginning of colPalette as long as colors 
                             are not in immediately proceeding row (some of the colors at the end of 
                             bigPalette are a bit wonky, and so if you have a large clusters matrix, 
                             this can be useful).")
          )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aTick"), value = FALSE, label = "Add tick?")),
      conditionalPanel(condition = paste0("input['", ns("aTick"), "']"),
          column(3, checkboxInput(ns("tick"), label = "TRUE", value = FALSE))
      ),
      column(2, checkboxInput(ns("hTick"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hTick"), "']"),
          column(4, helpText("Logical, whether to draw ticks on x-axis for each sample."))
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aYlab"), value = FALSE, label = "Add ylab?")),
      conditionalPanel(condition = paste0("input['", ns("aYlab"), "']"),
          column(3, textInput(ns("ylab"), label = NULL))
      ),
      column(2, checkboxInput(ns("hYlab"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hYlab"), "']"),
          column(4, helpText("Character string for the label of y-axis."))
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aXlab"), value = FALSE, label = "Add xlab?")),
      conditionalPanel(condition = paste0("input['", ns("aXlab"), "']"),
          column(3, textInput(ns("xlab"), label = NULL))
      ),
      column(2, checkboxInput(ns("hXlab"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hXlab"), "']"),
          column(4, helpText("Character string for the label of x-axis."))
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aXlab"), value = FALSE, label = "Add xlab?")),
      conditionalPanel(condition = paste0("input['", ns("aXlab"), "']"),
          column(3, numericInput(ns("axisLine"), value = 1, label = NULL))
      ),
      column(2, checkboxInput(ns("hXlab"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hXlab"), "']"),
                       column(4, helpText("Character string for the label of x-axis."))
      )
    ),
    
    fluidRow(
      column(4, 
             h3("Axis Line"),
             helpText("The number of lines in the axis labels on y-axis should be (passed to line = ... in the axis call)"),
             numericInput(ns("axisLine"), value = 1, label = NULL)
             
      ),
      column(2,
             h3("Box"),
             helpText("Logical, whether to draw a box arouns the plot"),
             checkboxInput(ns("box"), label = NULL, value = FALSE)
      )
    )
    
    
  )
}

makePlotClustersCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("plotClusters( cE" )
    if(input$aSampleData) {
      if(input$sampleData != 'NULL') {
        code <- paste(code, ", sampleData = c(", input$sampleData, ")", sep = "")
      } else {
        code <- paste(code, ", sampleData = ", input$sampleData, sep = "")
      }
    }
    
    if(input$aReuseColors) {
      code <- paste(code, ", reuseColors = ", input$reuseColors, sep = "")
    }
    
    if(input$aMatchToTop) {
      code <- paste(code, ", matchToTop = ", input$matchToTop, sep = "")
    }
    
    if(input$aUnassignedColor) {
      code <- paste(code, ", unassignedColor = '", input$unassignedColor, "'", sep = "")
    }
    
    if(input$aMissingColor) {
      code <- paste(code, "', missingColor = '", input$missingColor, "'", sep = "")
    }
    
    if(input$aMinRequireColor) {
      if(!is.na(input$minRequireColor) && (input$minRequireColor <= 100 && input$minRequireColor > 0)) {
        code <- paste(code, ", minRequireColor = ", input$minRequireColor)
      }
    }
    
    if(input$aStartNewColors) {
      code <- paste(code, ", startNewColors = ", input$startNewColors, sep = "")
    }
    
    if(input$aTick) {
      code <- paste(code, ", tick = ", input$tick, sep = "")
    }
    
    if(input$aYlab) {
      code <- paste(code, ", ylab = '", input$ylab, "'", sep = "")
    }
    
    if(input$aXlab) {
      code <- paste(code, ", xlab = '", input$xlab, "'", sep = "")
    }
    
    code <- paste(code, "', axisLine = ", input$axisLine, ", box = ", input$box)
    
    code <- paste(code, ")")
  })
  
  return(code)
  
}

plotClustersHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for plotClusters()"),
    helpText("helptext here")
  )
}

