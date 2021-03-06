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

singleNumericInput <- function(id, sidelabel, aboveLabel, val, defaultValue=NULL, help="No help yet available", required = FALSE) {
  ns <- NS(id)
  aVal<-paste("a",capwords(val),sep="")
  hVal<-paste("h",capwords(val),sep="")
  sidelabel<-convertSideLabel(sidelabel,val)
  if(!required) {
    fluidRow(
      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
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



vectorInput<-function(id,sidelabel, aboveLabel,val, defaultValue=NULL, help="No help yet available",required=FALSE){
	ns <- NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){ #for now, not implement, because don't know how to do the required version...
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
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

logicalInput<-function(id,sidelabel, val, help="No help yet available",required=FALSE){
	ns<-NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	          column(3, checkboxGroupInput(ns(val), label = "Can choose more than one", choices = c("TRUE", "FALSE")))
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
	      column(3,checkboxGroupInput(ns(val), label = "Can choose more than one", choices = c("TRUE", "FALSE"))),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	            column(4, helpText(help))
	      )
	    )
	}

}

multipleOptionsInput<-function(id, sidelabel,options,val, help="No help yet available",required=FALSE){
	ns<-NS(id) #If id argument to NS is missing, returns a function that expects an id string as its only argument and returns that id with the namespace prepended.


	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	         column(3, checkboxGroupInput(ns(val), choices = options, label = "Choose all of interest"))),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	      )
	      
	    )
	}
	else{
		
	    fluidRow(
	      column(3, sidelabel), #creates problem here, because need if required=FALSE, the value of ns(aVal) is set to be true in the input list...e.g. input$aDimReduce needs to be set to TRUE to be able to get the code set up to run (under function makeCode)
	      column(3,checkboxGroupInput(ns(val), choices = options, label = "Choose all of interest")),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	         
	      )
	    )
	}

}

singleOptionsInput<-function(id, sidelabel,options,val, help="No help yet available",required=FALSE){
	ns<-NS(id) #If id argument to NS is missing, returns a function that expects an id string as its only argument and returns that id with the namespace prepended.
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",capwords(val),sep="")
	hVal<-paste("h",capwords(val),sep="")
	sidelabel<-convertSideLabel(sidelabel,val)
	if(!required){
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	         column(3, selectInput(ns(val), choices = options, label = "Choose one",multiple=FALSE))),
	      column(2, checkboxInput(ns(hVal), value = FALSE, label = "Click here for help")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	             column(4, helpText(help))
	      )
	    )
	}
	else{
		
	    fluidRow(
	      column(3, sidelabel), #creates problem here, because need if required=FALSE, the value of ns(aVal) is set to be true in the input list...e.g. input$aDimReduce needs to be set to TRUE to be able to get the code set up to run (under function makeCode)
	      column(3,selectInput(ns(val), choices = options, label = "Choose one")),
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


