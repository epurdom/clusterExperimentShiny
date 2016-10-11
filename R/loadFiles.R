
#' @rdname InternalModules
#' @export
loadHelpText<-function(){
    "This tab contains important utilities, such as loading data, controling your default working directory, etc. "
}
# countInfo<-setWD <- function(id, label = "start message") {
#     ns <- NS(id)
#     tagList(
#         fluidRow(
#             checkboxInput("isCount", label = NULL, value = FALSE)
#             
#             logicalInput(id,sidelabel="Is the input data counts?", defaultValue=FALSE,val="isCount", help="Whether the data are in counts, in which case the default transFun argument is set as log2(x+1).",required=TRUE), 
#                 conditionalPanel(
#                 condition = setUpConditionalPanelTest( id, val="dimReduce", allOptions=dimReduceOptions, validOptions="PCA"),
#                 tags$hr(),
#                 logicalInput(id,sidelabel="Do you want to transform your data?", val="sequential", help="Whether the data are in counts, in which case the default transFun argument is set as log2(x+1).",required=FALSE, functionName="clusterMany"), 
#                 
#             ),
#             
#             
#             column(6,
#                    h3("Please enter a working directory for this Cluster Experiment session"),
#                    textInput(ns("workingDirectory"), label = "eg: 'homeDirectory/subdirectory/filename.r", 
#                              value = path.expand("~"), width = '100%')
#             )
#         )
#     )
# }
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

# Module server function
#' @rdname InternalModules
#' @export
#' @importFrom stringr str_sub
rdaFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    input$rdaFile
  })
  # The user's data, parsed into a data frame
  rda <- reactive({
    if(length(input$rdaFile[1]) > 0 && str_sub(input$rdaFile[1], start = -4) == ".rds") {
      holderObject <- readRDS(userFile()$datapath)
      if (class(holderObject)[1] == "SummarizedExperiment" || class(holderObject)[1] == "ClusterExperiment") {

        return(holderObject)
      }
    }
    
    else if(length(input$rdaFile[1]) > 0) {
      session$sendCustomMessage(type = 'helpMessage',
                                message = 'incorrect File format, please upload a file of type
                                .csv or an .rds file of class "SummarizedExperiment" or "clusterExperiment"')
    }
    else
      return(NULL)
    
    })
  # Return the reactive that yields the data frame
  return(rda)
} #end of dataframe function


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






# Module server function
#' @rdname InternalModules
#' @export
dataFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any

  userFile <- reactive({
    input$file
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    if(is.null(input$file)) {
      return(NULL)
    }
    
      if(input$rowNamesFirstCol) {
        return(read.csv(userFile()$datapath,
                        header = input$header,
                        sep = input$sep,
                        row.names = 1,
                        quote = "",
                        stringsAsFactors = stringsAsFactors))
      } else {
        return(read.csv(userFile()$datapath,
                        header = input$header,
                        sep = input$sep,
                        stringsAsFactors = stringsAsFactors))
      }
    
    })
  
  # Return the reactive that yields the data frame
  return(dataframe)
} #end of dataframe function

#Returns code to be written to script
#' @rdname InternalModules
#' @export
csvAssayCode <- function(input, output, session, stringsAsFactors) {
  userFile <- reactive({
    input$colData
  })
  
  text <- reactive({
    if(input$rowNamesFirstCol) {
      return(paste("read.csv('", userFile()$datapath, "', header = ", input$header, ", sep = '", input$sep,
             "', stringsAsFactors = ", stringsAsFactors, ", row.names = 1, quote = '')", sep = ""))
    } else {
      return(paste("read.csv(", userFile()$datapath, ", header = ", input$header, ", sep = ", input$sep,
             ", stringsAsFactors = ", stringsAsFactors, ", quote = '')", sep = ""))
    }
  })
  return(text)
}

# Module server function
#' @rdname InternalModules
#' @export
colDataFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    input$colData
  })
  
  # The user's data, parsed into a data frame, we will need to expand beyond .csv
  dataframe <- reactive({
    if(is.null(input$colData)) {
      return(NULL)
    }
      return(read.csv(userFile()$datapath,
                      header = input$colHeader,
                      sep = input$colSep,
                      stringsAsFactors = stringsAsFactors))
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
} #end of colDataFile function

#Returns code to be written to script
#' @rdname InternalModules
#' @export
csvColCode <- function(input, output, session, stringsAsFactors) {
  userFile <- reactive({
    input$colData
  })
  
  text <- reactive({
    return(paste("read.csv('", userFile()$datapath, "', header = ", input$colHeader, ", sep = '", 
                 input$colSep, "', stringsAsFactors = ", 
                 stringsAsFactors, ")", sep = ""))
  })
  return(text)
}

# Module server function
#' @rdname InternalModules
#' @export
rowDataFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    input$rowData
  })
  
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    if(is.null(input$rowData)) {
      return(NULL)
    }
      return(read.csv(userFile()$datapath,
                      header = input$rowHeader,
                      sep = input$rowSep,
                      stringsAsFactors = stringsAsFactors))
    })
  
  # Return the reactive that yields the data frame
  return(dataframe)
} #end of colDataFile function

#' @rdname InternalModules
#' @export
csvRowCode <- function(input, output, session, stringsAsFactors) {
  userFile <- reactive({
    input$rowData
  })
  text <- reactive({
  return(paste("read.csv('", userFile()$datapath, "', header = ", input$rowHeader, ", sep = '", 
               input$rowSep, "', stringsAsFactors = ", 
               stringsAsFactors, ")", sep = ""))
  })
  return(text)
}


