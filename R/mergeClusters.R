#module for emerge clusters
#' @rdname InternalModules
#' @export
mergeClustersInput <- function(id, label = "cMInputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h2("Inputs for Merge Clusters"),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aMergeMethod"), value = FALSE, label = "Add mergeMethod?")),
      conditionalPanel(condition = paste0("input['", ns("aMergeMethod"), "']"),
            column(3, radioButtons(ns("mergeMethod"), label = NULL,
                                   choices = c("none", "adjP", "locfdr", "MB", "JC")))
      ),
      column(2, checkboxInput(ns("hMergeMethod"), value = FALSE, label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hMergeMethod"), "']"),
            column(4, helpText("method for calculating proportion of non-null that will be used to
                               merge clusters (if 'none', no merging will be done). 'JC' refers to
                               the method of Ji and Cai (2007), and implementation of 'JC' method is
                               copied from code available on Jiashin Ji's website, December 16, 2015
                               (http://www.stat.cmu.edu/~jiashun/Research/software/NullandProp/). 
                               'locfdr' refers to the method of Efron (2004) and is implemented in the 
                               package locfdr. 'MB' refers to the method of Meinshausen and Buhlmann (2005)
                               and is implemented in the package howmany. 'adjP' refers to the proportion 
                               of genes that are found significant based on a FDR adjusted p-values (method
                               'BH') and a cutoff of 0.05. If mergeMethod is not equal to 'none' then the 
                               plotting will indicate where the clusters will be merged (assuming plotType 
                               is not 'none')."))
                       )
      ),
    
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aCutoff"), value = FALSE, label = "Add cutoff?")),
      conditionalPanel(condition = paste0("input['", ns("aCutoff"), "']"),
          column(3, numericInput(ns("cutoff"), label = NULL, value = .5))
      ),
      column(2, checkboxInput(ns("hCutoff"), value = FALSE, label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hCutoff"), "']"),
          column(4, helpText("minimimum value required for NOT merging a cluster, i.e. two clusters 
                             with the proportion of DE below cutoff will be merged. Must be a value 
                             between 0, 1, where lower values will make it harder to merge clusters.")
          )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aIsCount"), value = FALSE, label = "Add isCount?")),
      conditionalPanel(condition = paste0("input['", ns("aIsCount"), "']"),
          column(3, checkboxInput(ns("isCount"), label = "TRUE", value = FALSE))
      ),
      column(2, checkboxInput(ns("hIsCount"), value = FALSE, label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hIsCount"), "']"),
          column(4, helpText("logical as to whether input data is a count matrix. If isCount=TRUE, 
                             and the input is a matrix, log2(count + 1) will be used for makeDendrogram
                             and the original data with voom correction will be used in getBestFeatures).
                             If input is ClusterExperiment, then setting isCount=TRUE also means
                             that the log2(1+count) will be used as the transformation, like for the
                             matrix case as well as the voom calculation, and will NOT use the 
                             transformation stored in the object. If FALSE, then transform(x) will be
                             given to the input and will be used for both makeDendrogram and 
                             getBestFeatures, with no voom correction.")
          )
      )
    ),
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aClusterLabel"), value = FALSE, label = "Add clusterLabel?")),
      conditionalPanel(condition = paste0("input['", ns("aClusterLabel"), "']"),
          column(3, textInput(ns("clusterLabel"), label = NULL, value = "Enter label"))
      ),
      column(2, checkboxInput(ns("hClusterLabel"), value = FALSE, label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hClusterLabel"), "']"),
          column(4, helpText("a string used to describe the type of clustering. By default it is 
                             equal to 'mergeClusters', to indicate that this clustering is the result 
                             of a call to mergeClusters.")
          )
      )
    )
  )
}
#make code
#' @rdname InternalModules
#' @export
makeMergeClustersCode <- function(input, output, session, stringsAsFactors) {
  code <- reactive({
    code <- paste("cE <- mergeClusters(cE")
    if(input$aMergeMethod)
      code <- paste(code, ", mergeMethod = '", input$mergeMethod, "'", sep = "")
    if(input$aCutoff)
      code <- paste(code, ", cutoff = ", input$cutoff, sep = "")
    if(input$aIsCount)
      code <- paste(code, ", isCount = ", input$isCount, sep = "")  
    if(input$aClusterLabel)
      code <- paste(code, ", clusterLabel = '", input$clusterLabel, "'", sep = "")  
    code <- paste(code, ")", sep = "")
  })
  
  return(code)
}
#helptext
#' @rdname InternalModules
#' @export
mergeClustersHelpText <- function(id, label = "help title and text") {
  ns <- NS(id)
  tagList(
    h3("Specialized Inputs for MergeClusters()"),
    helpText("helptext here")
  )
}
