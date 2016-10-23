#save object tab
#' @rdname InternalModules
#' @export
saveObjectMessage <- function(id, label = "start message") {
  ns <- NS(id)
  tagList(
    h2("Save current clusterExperiment Object:"),
    fluidRow(
        column(6, 
            h4("Please enter file path and name (of type .rds) of internal clusterExperiment object to be saved (via 'saveRDS'):"),
            textInput(ns("saveObjectPath"), label = "eg: 'homeDirectory/subdirectory/objectFileName.rds", 
                #how does this work???
                      #value= makeFileWithDateTag(file="clusterObject_final.rds",wd=input[["fileInput-workingDirectory"]]), width = '100%')
            value = "~/clusterExperimentShinyAppOutput/clusteringObject.rda", width = '100%')
        )
    ),
    actionButton(ns("createObject"), label = "Save Object")
  )
}

#function to be called by UI to save object if autosave on
saveObjects<-function(path,type=c("sE","cE"),recordCode=FALSE){
    type<-match.arg(type)
    sE<-get("sE",envir=appGlobal)
    cE<-get("cE",envir=appGlobal)
    filePath<-get("filePath",envir=appGlobal)
    if(type=="sE") saveRDS(sE, path)
    if(type=="cE") saveRDS(cE, path)
    if(recordCode) {
        code<-paste("saveRDS(",type,", '",path,"')",sep="")
        recordCodeFun(code=code,tag="Save Object")
    }
}