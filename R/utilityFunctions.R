###Plan:
#Make global list of required parameters
#functions search global list of parameters that are required to decide if required or not
#create function to return TRUE/FALSE for makeCode to tell it whether to add entry; the function should look at val and aVal so that only adds it if both are defined. That way if unclick it, will take it out of code.
#Also make it look up the default values in clusterExperiment and put those in as default in all. Might need to make separate look up for RSEC versus clusterMany.


###########
## Functions dealing with assigning global values to the appGlobal environment
###########
#' @name InternalModules
#' @export
assignGlobal<-function(name,value){
    assign(name,value=value,envir=appGlobal)
    return(value)
}
#' @rdname InternalModules
#' @export
# only works if doesn't need local variables...only global 
runCodeAssignGlobal<-function(codeText,recordCode,recordTag){
    #####
    #make sure updated values
    sE<-get("sE",envir=appGlobal)
    cE<-get("cE",envir=appGlobal)
    ######
    divString<-strsplit(codeText,"<-")
    nam<-divString[[1]][1] #may need to make sure remove white space
    nam<-gsub("[[:space:]]","",nam)
    # browser()
    if(!nam %in% c("cE","sE","filePath","makeFile")){
        browser()
        stop("invalid global parameter assigned.")
    }
    command<-divString[[1]][2]
    command<-gsub("[[:space:]]","",command)
    assign(nam,value=eval(parse(text = command)),envir=appGlobal)
    invisible(get(nam,appGlobal))
    if(recordCode){
        recordCodeFun(code=codeText,tag=recordTag,section=TRUE)
    }
}

#' @rdname InternalModules
#' @export
recordCodeFun<-function(code,tag,section=FALSE){
    file<-get("filePath",envir=appGlobal)
    if(section) startText<-sprintf("\n###########\n### %s: \n###########",tag)
    else startText<-sprintf("#--- %s: ---#",tag)
    cat(startText,code, sep="\n", file = file, append = TRUE)
}

##########
## Parse whether values have been changed and create new code
#########
#' @rdname InternalModules
#' @export
#' @details addArguments pastes additional arguments for constructing code.
addArguments<-function(input,currCode,val,isCharacter=TRUE){
    #This doesn't work, because vector input is actually just single input string!
#     if(length(input[[val]])>1){
#         if(isCharacter) inputvals<-paste("c('",paste(input[[val]],sep="",collapse="','"),"')",sep="")
#         else inputvals<-paste("c(",paste(input[[val]],sep="",collapse=","),")",sep="")
#     }
#     else{
#         if(isCharacter) inputvals<-paste("'",input[[val]],"'",sep="")
#         else inputvals<-input[[val]]
#     }
            if(isCharacter) inputvals<-paste("c('",paste(input[[val]],sep="",collapse="','"),"')",sep="")
            else inputvals<-paste("c(",paste(input[[val]],sep="",collapse=","),")",sep="")
    paste(currCode, ", ",val,"=", inputvals, sep = "")
}
#' @rdname InternalModules
#' @export
#' @details testArguments tests whether arguments have been checked
testArguments<-function(input,val){
    aVal<-paste("a",capwords(val),sep="")
    nms<-names(input)
    logic<-val %in% nms && !is.null(input[[val]]) && !is.na(input[[val]])
    #-----
    #If conditional argument, determine whether user has clicked on checkbox. Otherwise, only depends on the value of val
    #if required, the 'a' value is set to null
    #-----
    if(logic && aVal%in%nms && !is.null(input[[aVal]])) logic<-logic && input[[aVal]] 
    if(logic && is.character(input[[val]])) logic<-logic & all(input[[val]]!="")
    
    #logical and multiple choice are NULL if not picked
    #numeric is NA
    #vector input is ""
    return(logic)
}
#' @rdname InternalModules
#' @export
combineArgs<-function(input, currCode,val,isCharacter=TRUE){
    if(testArguments(input,val)) currCode<-addArguments(input,currCode,val,isCharacter)
    return(currCode)
}


##########
## Small helper functions
#########

#change a file like XYZ.abc into XYZ_08132016.abc
#' @rdname InternalModules
#' @export
makeFileWithDateTag<-function(file,wd){
    fileSep<-strsplit(file,"[.]")
    file<-fileSep[[1]][[1]]
    ext<-fileSep[[1]][[2]]
    file.path(wd,paste0(file,"_",format(Sys.Date(),"%m%d%Y"),".",ext,collapse=""))
}











