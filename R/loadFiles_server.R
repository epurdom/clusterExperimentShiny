#----------------
# Function to capture isCount and transFun
# this is just simple module that gets the input so that another module can use it
# good reference on modules using other modules https://www.r-bloggers.com/shiny-module-design-patterns-pass-module-inputs-to-other-modules/
#----------------
#' @rdname InternalModules
#' @export
getIsCount <- function(input, output, session) {
    input
}

# Module server function
#' @rdname InternalModules
#' @export
#' @importFrom stringr str_sub
rdaFile <- function(input, output, session, stringsAsFactors,recordCode=FALSE,recordFile) {
    # EAP: Why is this needed? Why not just call input directly?
    userFile <- reactive({
        # If no file is selected, don't do anything
        input$rdaFile
    })
    # The user's data, parsed into a data frame
    rda <- reactive({
        if(length(input$rdaFile[1]) > 0 && str_sub(input$rdaFile[1], start = -4) == ".rds") {
           # browser()
            #holderObject <- eval(parse(text = code))
            holderObject <- readRDS(userFile()$datapath)
            if (class(holderObject)[1] == "SummarizedExperiment" || class(holderObject)[1] == "ClusterExperiment") {
                if(recordCode){
                    code<-paste("readRDS('",userFile()$name,"')",sep="") #different name of what upload in code versus right name
                    recordCodeFun(code=code,tag="Loading rds object",section=TRUE)
                }
                return(holderObject)
            }
            else session$sendCustomMessage(type = 'helpMessage',
                  message = 'incorrect object type, please upload an object of class "SummarizedExperiment" or "clusterExperiment"')
            
        }
        else if(length(input$rdaFile[1]) > 0) {
            session$sendCustomMessage(type = 'helpMessage',
                   message = 'incorrect File format, please upload a file of type .rds containing an object of class "SummarizedExperiment" or "clusterExperiment"')
        }
        else return(NULL)
        })
    # Return the reactive that yields the data frame
    return(rda)
} 


#Returns code to be written to script
#' @rdname InternalModules
#' @export
readCSVInput <- function(input, output, session,recordCode=FALSE,whichData,recordFile,recordSection=FALSE,nrows=-1) {
    stringsAsFactors<-FALSE
    if(whichData=="file"){
        comment<-"load assay"
        name<-"assay"
    }
    if(whichData=="colData"){
        comment<-"load column data"
        name<-"colData"
    }
    if(whichData=="rowData"){
        comment<-"load row data"
        name<-"rowData"
    }
    # The selected file, if any
    userFile <- reactive({
        input[[whichData]]
    })
    
    # The user's data, parsed into a data frame
    dataframe <- reactive({
        if(is.null(userFile())) {
            return(NULL)
        }
        else{
            codeOptions<-sprintf("header=%s, sep='%s', stringsAsFactors=%s,quote='',nrows=%s",
                                 input$header,input$sep,stringsAsFactors,nrows)
            if(input$rowNamesFirstCol) codeOptions<-paste(codeOptions,", row.names=",1,sep="")
            codeWrite<-paste("assay<-read.csv('",userFile()$name,"',",codeOptions,")",sep="")
            codeRun<-paste("read.csv('",userFile()$datapath,"',",codeOptions,")",sep="")
            if(recordCode) recordCodeFun(code=codeWrite,tag=comment,section=recordSection)
            return(eval(parse(text = codeRun)))
        }
    })
    
}

#Returns code to be written to script
#' @rdname InternalModules
#' @export
createSummarizedExp<-function(assay,colData,rowData,recordCode,recordTag,recordFile){
    #if statements to add the correct, non-null data frames to sE
    if(!is.null(assay) && !is.null(colData) && !is.null(rowData)) {
        code<-"SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData),rowData = data.matrix(rowData))"
    }
    else if (!is.null(assay) && !is.null(colData) && is.null(rowData)) {
        code<-"SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData))"
    } else if(!is.null(assay) && is.null(colData) && !is.null(rowData)) {
        code<-"SummarizedExperiment(assays = data.matrix(assay), rowData = data.matrix(rowData))"
    } else if(!is.null(assay) && is.null(colData) && is.null(rowData)) {
        code<-"SummarizedExperiment(assays = data.matrix(assay))"
    } 
    else {
        return(HTML(paste("Error, need to upload data file")))
    }
    if(recordCode) recordCodeFun(code=sprintf("sE <- %s",code),tag=recordTag)
    return(eval(parse(text = code)))

}
