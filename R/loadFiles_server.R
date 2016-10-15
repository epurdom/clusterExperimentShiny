#################
# Capture isCount and transFun
# good reference on modules using other modules https://www.r-bloggers.com/shiny-module-design-patterns-pass-module-inputs-to-other-modules/
#################
#' @rdname InternalModules
#' @export
getIsCount <- function(input, output, session) {
    #     result <- reactive({
    #         isCount<-if(testArguments(input,"isCount")) input['isCount'] else FALSE
    #         transFun<-if(testArguments(input,"transFun")) input['transFun'] else NULL
    #         result<-list(isCount=isCount,transFun=transFun)
    #         result
    #     })
    #     result
    input
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

