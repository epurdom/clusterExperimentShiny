
#' @rdname InternalModules
#' @export
loadHelpText<-function(){
    p("This tab concerns important utilities, such as loading data, controling your default working directory, etc. ")
}
#' @rdname InternalModules
#' @export
countInfo<-function(id, label = "count and transformation decisions") {
    ns <- NS(id)
    tagList(
        logicalInput(id,sidelabel="Is the input data counts?", 
                         defaultValue=FALSE,val="isCount",required=TRUE, multipleAllowed=FALSE,
                         help="Whether the data are in counts, in which case the data is transformed with log2(x+1)."), 
#         conditionalPanel( #why doesn't this work? Also doesn't work if make them character values...
#                 condition = setUpConditionalPanelTest( id, val="isCount", allOptions=c(TRUE,FALSE), validOptions=FALSE),
#                   vectorInput(id,sidelabel="Do you want to specify a transformation?", aboveLabel="e.g. function(x){x}",val="transFun", 
#            help="Give function that should be applied to the uploaded data matrix. Will over-ride choice of counts (above). Can be used to provide different offset in the log, for example.", functionName="clusterMany")
#              )
        vectorInput(id,sidelabel="Do you want to specify a transformation?", aboveLabel="e.g. function(x){x}",val="transFun", 
                    help="Give function that should be applied to the uploaded data matrix. Will over-ride choice of counts (above). Can be used to provide different offset in the log, for example.", functionName="clusterMany")
        
    )
}


#Function that sets working directory
#' @rdname InternalModules
#' @export
setWD <- function(id, label = "start message") {
  ns <- NS(id)
  tagList(
      fluidRow(
        column(6,
            h3("Please enter a working directory for this Cluster Experiment session"),
            textInput(ns("workingDirectory"), label = "eg: 'homeDirectory/subdirectory/filename.r", 
                      value = path.expand("~"), width = '100%')
        )
      )
  )
}


#functions that uploads a rda file
#' @rdname InternalModules
#' @export
rdaFileInput <- function(id, label = "upload rds file") {
  ns <- NS(id)
  fluidRow(
    column(12,
           h3("Upload an object"),
           helpText("Choose a SummarizedExperiment or clusterExperiment object (saved as an .rds file) to upload:"),
           fileInput(ns("rdaFile"), label = NULL, accept = ".rds")
    )
  )
}



#input function for csv assay file
#' @rdname InternalModules
#' @export
csvAssay <- function( id, label = "upload .csv file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             h3("Choose a Data File to Upload:"),
             helpText("Upload the data on which to run the clustering. Can be: matrix (with genes in rows), 
                      a list of datasets overwhich the clusterings should be run, a SummarizedExperiment object, 
                      or a ClusterExperiment object."),
             fileInput(ns("file"), label = NULL,
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
             ),
      column(3,
             h3(""),
             radioButtons(ns('sep'), 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ',')
      ),
      column(3,
             h3(""),
             checkboxInput(ns('rowNamesFirstCol'), label = 'Are row names in first col?', value = TRUE)
      ),
      column(3,
             h3(""),
             checkboxInput(ns('header'), 'Header')
      )
    )
  )
}

#input function for csv col file input
#' @rdname InternalModules
#' @export
csvColData <- function( id, label = "upload .csv file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             h3("Choose column data upload:"),
             helpText("Upload the data associated with the columns. Optional, 
                      but reccomended for Bioconductor related projects."),
             fileInput(ns("colData"), label = NULL,
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
             ),
      column(3,
             h3(""),
             radioButtons(ns('colSep'), 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ',')
      ),
      column(3,
             h3(""),
             checkboxInput(ns('colRowNamesFirstCol'), label = 'Are row names in first col?', value = TRUE)
      ),
      column(3,
             h3(""),
             checkboxInput(ns('colHeader'), 'Header')
      )
    )
  )
}

#input function for csv col file input
#' @rdname InternalModules
#' @export
csvRowData <- function( id, label = "upload .csv file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(3,
             h3("Choose Row data upload:"),
             helpText("Upload the data associated with the rows. Optional."),
             fileInput(ns("rowData"), label = NULL,
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv'
                       )
             )
      ),
      column(3,
             h3(""),
             radioButtons(ns('rowSep'), 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ',')
      ),
      column(3,
             h3(""),             
             checkboxInput(ns('rowRowNamesFirstCol'), label = 'Are row names in first col?', value = TRUE)
      ),
      column(3,
             h3(""),
             checkboxInput(ns('rowHeader'), 'Header')
      )
    )
  )
}







