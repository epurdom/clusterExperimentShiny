



plotClusterInput <- function(id, label = "plotCluster inputs") {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, 
             h3("Which Clusters"),
             helpText("waiting on discussion with Elizabeth")
      ),
      
      column(3,
             h3("Sample Data"),
             helpText("If clusters is a matrix, sampleData gives a matrix of additional cluster/sampleData on the samples 
                      to be plotted with the clusterings given in clusters. Values in sampleData will be added to the end 
                      (bottom) of the plot. If clusters is a ClusterExperiment object, sampleData must be either an index 
                      or a character vector that references a column or column name, respectively, of the colData slot of
                      the ClusterExperiment object. Please enter either a sequence of numbers separated by commas, or leave
                      as 'NULL' if you wish for nothing to be passed to this argument. UNFINISHED (NULL is inelegant)"),
             textInput(ns("sampleData"), label = NULL, value = "NULL")
      ),
      column(3,
             h3("Reuse Colors"),
             helpText("Whether each row should consist of the same set of colors. By default (FALSE) each cluster that the
                      algorithm doesn't identify to the previous rows clusters gets a new color."),
             checkboxInput(ns("reuseColors"), value = FALSE, label = NULL)
      ),
      column(3,
             h3("Match To Top"),
             helpText("Logical as to whether all clusters should be aligned to the first row. By default (FALSE) each 
                      cluster is aligned to the ordered clusters of the row above it."),
             checkboxInput(ns("matchToTop"), value = FALSE, label = NULL)
      )
    ),
    fluidRow(
      column(3, 
             h3("Unassigned Color"),
             helpText("UNFINISHED: What form is color supposed to be in? Default is white?
                      If “-1” in clusters, will be given this color (meant for samples not assigned to cluster)."),
             radioButtons(ns("unassignedColor"), choices = c("white", "black", "grey"), label = NULL, selected = "white")
      ),
      column(3, 
             h3("Missing Color"),
             helpText("UNFINISHED: What form is color supposed to be in? Default?
                      If “-1” in clusters, will be given this color (meant for samples not assigned to cluster)."),
             radioButtons(ns("missingColor"), choices = c("white", "black", "grey"), label = NULL, selected = "white")
      ),
      column(2, 
             h3("Minimun Required Color"),
             helpText("UNFINISHED- proportion or percent? 
                      In aligning colors between rows of clusters, require this percent overlap."),
             numericInput(ns("minRequireColor"), label = NULL, value = 100)
      ),
      column(4, 
             h3("Start New Colors"),
             helpText("logical, indicating whether in aligning colors between rows of clusters, should the colors restart 
                      at beginning of colPalette as long as colors are not in immediately proceeding row (some of the 
                      colors at the end of bigPalette are a bit wonky, and so if you have a large clusters matrix, 
                      this can be useful)."),
             checkboxInput(ns("startNewColors"), value = FALSE, label = NULL)
             
      )
    ),
    fluidRow(
      column(2, 
             h3("Tick"),
             helpText("Logical, whether to draw ticks on x-axis for each sample."),
             checkboxInput(ns("tick"), label = NULL, value = FALSE)
      ),
      column(2, 
             h3("Y Axis Label"),
             helpText("Character string for the label of y-axis."),
             textInput(ns("ylab"), label = NULL)
      ),
      column(2, 
             h3("X Axis Label"),
             helpText("character string for the label of x-axis."),
             textInput(ns("xlab"), label = NULL)
      ),
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
    if(input$sampleData != 'NULL') {
      code <- paste(code, ", sampleData = c(", input$sampleData, ")")
    } else {
      code <- paste(code, ", sampleData = ", input$sampleData)
    }
    code <- paste(code, ", reuseColors = ", input$reuseColors, ", matchToTop = ", input$matchToTop, 
                  ", unassignedColor = '", input$unassignedColor, "', missingColor = '", input$missingColor, "'", sep = "")
    if(!is.na(input$minRequireColor) && (input$minRequireColor < 100 && input$minRequireColor > 0)) {
      code <- paste(code, ", minRequireColor = ", input$minRequireColor)
    }
    
    code <- paste(code, ", startNewColors = ", input$startNewColors, ", tick = ", input$tick, ", ylab = '", input$ylab, "'",
                  ", xlab = '", input$xlab, "', axisLine = ", input$axisLine, ", box = ", input$box)
    
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

