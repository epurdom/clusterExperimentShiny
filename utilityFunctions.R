###Plan:
#Make global list of required parameters
#functions search global list of parameters that are required to decide if required or not
#create function to return TRUE/FALSE for makeCode to tell it whether to add entry; the function should look at val and aVal so that only adds it if both are defined. That way if unclick it, will take it out of code.
#Also make it look up the default values in clusterExperiment and put those in as default in all. Might need to make separate look up for RSEC versus clusterMany.


##########
## Parse whether values have been changed and create new code
#########
addArguments<-function(input,currCode,val,isCharacter=TRUE){
	if(isCharacter) paste(currCode, ", ",val,"=c('",paste(input[[val]],sep="",collapse="','"), "')", sep = "")
		else paste(currCode, ", ",val,"=c(",paste(input[[val]],sep="",collapse=","), ")", sep = "")
}
testArguments<-function(input,val){
    aVal<-paste("a",capwords(val),sep="")
	nms<-names(input)
	logic<-val %in% nms && !is.null(input[[val]]) && !is.na(input[[val]])
	#-----
	#If conditional argument, determine whether user has clicked on checkbox. Otherwise, only depends on the value of val
	#if required, the 'a' value is set to null
	#-----
	if(logic && aVal%in%nms && !is.null(input[[aVal]])) logic<-logic && input[[aVal]] 
	if(logic && is.character(input[[val]])) logic<-logic & input[[val]]!=""		
	
	#logical and multiple choice are NULL if not picked
	#numeric is NA
	#vector input is ""
	
	return(logic)
}
combineArgs<-function(input, currCode,val,isCharacter=TRUE){
	if(testArguments(input,val)) currCode<-addArguments(input,currCode,val,isCharacter)
		return(currCode)
}


##########
## Small helper functions
#########
#from help of tolower/toupper
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

##Add to each side label, the argument in clusterMany it corresponds to (i.e. val)
#not sure about how it looks right now, but easy to undo or fix globally this way.
convertSideLabel<-function(sidelabel,val){	paste0(sidelabel,paste0("(",val,")",collapse=""),collapse="\n") 
}

#from this conversation:
#http://r.789695.n4.nabble.com/Q-Get-formal-arguments-of-my-implemented-S4-method-td4702420.html
methodFormals <- function(f, signature = character()) {
    fdef <- getGeneric(f)
    method <- selectMethod(fdef, signature)
    genFormals <- base::formals(fdef)
    b <- body(method)
    if(is(b, "{") && is(b[[2]], "<-") && identical(b[[2]][[2]], as.name(".local"))) {
        local <- eval(b[[2]][[3]])
        if(is.function(local))
            return(formals(local))
        warning("Expected a .local assignment to be a function. Corrupted method?")
    }
    genFormals
}

#assumes the base version is matrix. Need to check this for each code and update if not.
getAdditionalDefaults<-function(class,functionName,ceDefaults){

		matDefaults<-try(methodFormals("clusterMany",class),silent=TRUE)
		if(!inherits(matDefaults,"try-error")){
			#get rid of overlapping
			nmsMat<-names(matDefaults)
			nmsCE<-names(ceDefaults)
			matDefaults<-matDefaults[which(!nmsMat %in%nmsCE)]
			if(length(matDefaults)>0) ceDefaults<-c(matDefaults,ceDefaults)
		}
		return(ceDefaults)
	

}
findDefaults<-function(val,functionName){
	if(class(get(functionName))=="function"){ #S3
		ceDefaults<-as.list(args(functionName))
	}
	else{
		#return(NULL)
		ceDefaults<-methodFormals(functionName,signature="ClusterExperiment")
		ceDefaults<-getAdditionalDefaults("matrix",functionName,ceDefaults)
		ceDefaults<-getAdditionalDefaults("list",functionName,ceDefaults)
	}
	if(val%in% names(ceDefaults)){
		if(!all(is.na(ceDefaults[[val]])) && !is.null(ceDefaults[[val]]) && all(ceDefaults[[val]]=="")) return(NULL) else return(ceDefaults[[val]])
	}
	else return(NULL)
			
}
##########
## Conditional panels for arguments.
#########

singleNumericInput <- function(id, sidelabel, aboveLabel, val, defaultValue=NULL, help="No help yet available", required = FALSE,checkbox=FALSE,functionName) {
#	if(val=="ncores") browser()
  if(is.null(defaultValue)) defaultValue<-findDefaults(val,functionName)
  ns <- NS(id)
  aVal<-paste("a",capwords(val),sep="")
  hVal<-paste("h",capwords(val),sep="")
  sidelabel<-convertSideLabel(sidelabel,val)
  if(!required) {
    fluidRow(
      column(3, checkboxInput(ns(aVal), value = checkbox, label = sidelabel)),
      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
          column(3, numericInput(ns(val), label = aboveLabel, value = defaultValue))
      ),
      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
          column(4, helpText(help))
      )
    )
  } else {
    fluidRow(
      column(3, sidelabel), #creates problem here, because need if required=FALSE, the value of ns(aVal) is set to be true in the input list...e.g. input$aDimReduce needs to be set to TRUE to be able to get the code set up to run (under function makeCode)
      column(3, numericInput(ns(val), label = aboveLabel, value = defaultValue)),
      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
                       column(4, helpText(help))
                       
      )
    )
  }
  
}



vectorInput<-function(id,sidelabel, aboveLabel,val, defaultValue="", help="No help yet available",required=FALSE,checkbox=FALSE,functionName){
    if(is.null(defaultValue)) defaultValue<-findDefaults(val,functionName)
  ns <- NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){ #for now, not implement, because don't know how to do the required version...
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = checkbox, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	                       column(3, textInput(ns(val), label = aboveLabel, value = defaultValue))
	      ),
	      column(2, checkboxInput(ns(hVal), value = FALSE, 
	                              label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	          column(4, helpText(help)
	          )
	      )
		)
	}
	else{
	    fluidRow(
	      column(3, sidelabel),
		  #column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
	      column(3, textInput(ns(val), label = aboveLabel, value = defaultValue)),
	      column(2, checkboxInput(ns(hVal), value = FALSE, 
	                              label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	          column(4, helpText(help)
	          )
	      )
		)
	}
	

}

logicalInput<-function(id,sidelabel, val, help="No help yet available",required=FALSE,checkbox=FALSE,defaultValue=NULL,functionName){
    if(is.null(defaultValue)) defaultValue<-findDefaults(val,functionName)
  ns<-NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	if(is.logical(defaultValue)) defaultValue<-as.character(defaultValue)
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = checkbox, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	          column(3, checkboxGroupInput(ns(val), label = "Can all of interest", choices = c("TRUE", "FALSE"),selected=defaultValue))
	      ),
	      column(2, checkboxInput(ns(hVal), value = FALSE, 
	                                  label = "Click here for help")
	      ),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	            column(4, helpText(help))
	      )
	    )
	}
	else{
	    fluidRow(
	      column(3, sidelabel),
	      column(3,checkboxGroupInput(ns(val), label = "Choose all of interest", choices = c("TRUE", "FALSE")),selected=defaultValue),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	            column(4, helpText(help))
	      )
	    )
	}

}

multipleOptionsInput<-function(id, sidelabel,options,val, help="No help yet available",required=FALSE,checkbox=FALSE,defaultValue=NULL,functionName){
    if(is.null(defaultValue)) defaultValue<-findDefaults(val,functionName)
    ns<-NS(id) #If id argument to NS is missing, returns a function that expects an id string as its only argument and returns that id with the namespace prepended.


	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = checkbox, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	         column(3, checkboxGroupInput(ns(val), choices = options, label = "Choose all of interest",selected=defaultValue))),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	      )
	      
	    )
	}
	else{
		
	    fluidRow(
	      column(3, sidelabel), #creates problem here, because need if required=FALSE, the value of ns(aVal) is set to be true in the input list...e.g. input$aDimReduce needs to be set to TRUE to be able to get the code set up to run (under function makeCode)
	      column(3,checkboxGroupInput(ns(val), choices = options, label = "Choose all of interest",selected=defaultValue)),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	         
	      )
	    )
	}

}

singleOptionsInput<-function(id, sidelabel,options,val, help="No help yet available",required=FALSE,checkbox=FALSE,defaultValue=NULL,functionName){
    if(is.null(defaultValue)) defaultValue<-findDefaults(val,functionName)
  ns<-NS(id) #If id argument to NS is missing, returns a function that expects an id string as its only argument and returns that id with the namespace prepended.
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = checkbox, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	         column(3, selectInput(ns(val), choices = options, label = "Choose one",multiple=FALSE,selected=defaultValue))),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	      )
	    )
	}
	else{
		
	    fluidRow(
	      column(3, sidelabel), #creates problem here, because need if required=FALSE, the value of ns(aVal) is set to be true in the input list...e.g. input$aDimReduce needs to be set to TRUE to be able to get the code set up to run (under function makeCode)
	      column(3,selectInput(ns(val), choices = options, label = "Choose one",selected=defaultValue,multiple=FALSE)),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	         
	      )
	    )
	}

}

#based on suggestion in https://groups.google.com/forum/#!searchin/shiny-discuss/conditionalPanel(condition$20$3D$20paste0(%22input%5B$27%22$2C$20ns(%7Csort:relevance/shiny-discuss/ZItFs3014YE/PPY_8XIiBwAJ
setUpConditionalPanelTest<-function(id,val,allOptions, validOptions){
	ns<-NS(id)
	nAll<-length(allOptions)
	startStr<-sapply(0:(nAll-1),function(i){
		paste0("input['",ns(val),"'][",i,"]==",collapse="")
	})
	endString<-sapply(validOptions,function(x){paste0("'",x,"'",collapse="")})
	#take all combinations of these
	combs<-expand.grid(startStr,endString,stringsAsFactors=FALSE)
	totalStr<-paste0(apply(combs,1,paste0,collapse=""),collapse="||")
	return(totalStr)
    
}


