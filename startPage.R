startPageBasics <- function( id, label = "upload file") {
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
             radioButtons(ns('sep'), 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
             radioButtons(ns('quote'), 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"'),
             checkboxInput(ns('header'), 'Header')
      ),
      column(3,
 
              h3("Are Data in Counts?"),
              helpText("Whether the data are in counts, in which case the default transFun argument is set as log2(x+1). 
                       This is simply a convenience to the user."),
              checkboxInput(ns("isCount"), label = NULL, value = FALSE)
      ),
      column(3,
             h3("transform function"))
    ),
    fluidRow(
      column(4,
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
                         '.tsv',
                         '.rda'
                       )
             )
             ),
      column(3,
             radioButtons(ns('colSep'), 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
             radioButtons(ns('colQuote'), 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"'),
             checkboxInput(ns('colHeader'), 'Header')
      )
    ),
    fluidRow(
      column(4,
             h3("Choose column data upload:"),
             helpText("Upload the data associated with the rows. Optional."),
             fileInput(ns("rowData"), label = NULL,
                       accept = c(
                         'text/csv',
                         'text/comma-separated-values',
                         'text/tab-separated-values',
                         'text/plain',
                         '.csv',
                         '.tsv',
                         '.rda'
                       )
             )
             ),
      column(3,
             radioButtons(ns('rowSep'), 'Separator',
                          c(Comma=',',
                            Semicolon=';',
                            Tab='\t'),
                          ','),
             radioButtons(ns('rowQuote'), 'Quote',
                          c(None='',
                            'Double Quote'='"',
                            'Single Quote'="'"),
                          '"'),
             checkboxInput(ns('rowHeader'), 'Header')
      )
  )
    
  )
  
}






# Module server function
dataFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  # The user's data, parsed into a data frame, we will need to expand beyond .csv
  dataframe <- reactive({
    
    if(length(input$file[1]) > 0 && str_sub(input$file[1], start = -4) == ".csv") {
     return(read.csv(userFile()$datapath,
               header = input$header,
               sep = input$sep,
               quote = input$quote,
               stringsAsFactors = stringsAsFactors))
    }
    else if(length(input$file[1]) > 0 && str_sub(input$file[1], start = -4) == ".rda") {
      holderObject <- readRDS(userFile()$datapath)
      if (class(holderObject)[1] == "SummarizedExperiment" || class(holderObject)[1] == "clusterExperiment")
        return(holderObject)
    }
    
    else if(length(input$file[1]) > 0) {
      session$sendCustomMessage(type = 'helpMessage',
                                message = 'incorrect File format, please upload a file of type
                                .csv or an .rda file of class "SummarizedExperiment" or "clusterExperiment"')
    }
    else
      return(NULL)
    
    })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
} #end of dataframe function


# Module server function
colDataFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$colData, message = FALSE))
    input$colData
  })
  
  # The user's data, parsed into a data frame, we will need to expand beyond .csv
  dataframe <- reactive({
    
    if(length(input$colData[1]) > 0 && str_sub(input$colData[1], start = -4) == ".csv") {
      return(read.csv(userFile()$datapath,
                      header = input$colHeader,
                      sep = input$colSep,
                      quote = input$colQuote,
                      stringsAsFactors = stringsAsFactors))
    }
    else if(length(input$file[1]) > 0 && str_sub(input$file[1], start = -4) == ".rda") {
      holderObject <- readRDS(userFile()$datapath)
      if (class(holderObject)[1] == "SummarizedExperiment" || class(holderObject)[1] == "clusterExperiment")
        return(holderObject)
    }

    else if(length(input$file[1]) > 0) {
      session$sendCustomMessage(type = 'helpMessage',
                                message = 'incorrect File format, please upload a file of type
                                .csv or an .rda file of class "SummarizedExperiment" or "clusterExperiment"')
    }
    return(NULL)
    
  })
  
  # We can run observers in here if we want to
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  
  # Return the reactive that yields the data frame
  return(dataframe)
} #end of colDataFile function


makeCECode <- function(input, output, session, stringsAsFactors) {
  cECode <- reactive({
  
    cECode <- paste("ce <- clusterMany(", gsub('[.][A-z ]*', '', input$file[1]), 
                           ", isCount = ", input$isCount, sep = "")
    cECode
  })

return(cECode)

} # end of makeCECode function