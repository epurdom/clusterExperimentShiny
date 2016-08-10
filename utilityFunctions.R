vectorInput<-function(id,sidelabel, aboveLabel,val, defaultValue=NULL, help="No help yet available",required=FALSE){
	ns <- NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",val,sep="")
	hVal<-paste("h",val,sep="")
#	if(!required){ #for now, not implement, because don't know how to do the required version...
	    fluidRow(
	      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
	      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
	                       column(3, textInput(ns(val), label = aboveLabel, value = defaultValue))
	      ),
	      column(2, checkboxInput(ns(hVal), value = FALSE, 
	                              label = "Help Text and Instructions")),
	      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
	          column(4, helpText(help)
	          )
	      )
		)
#	}
	

}

logicalInput<-function(id,sidelabel, val, help="No help yet available",required=FALSE){
	ns<-NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",val,sep="")
	hVal<-paste("h",val,sep="")
    fluidRow(
      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
          column(3, checkboxGroupInput(ns(val), label = "Can choose more than one", choices = c("TRUE", "FALSE")))
      ),
      column(2, checkboxInput(ns(hVal), value = FALSE, 
                                  label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
            column(4, helpText(help))
      )
    )
}

multipleOptionsInput<-function(id, sidelabel,options,val, help="No help yet available",required=FALSE){
	ns<-NS(id)
	##Should be able to do this and not require user define these terms.
	aVal<-paste("a",val,sep="")
	hVal<-paste("h",val,sep="")
    fluidRow(
      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
         column(3, checkboxGroupInput(ns(val), choices = options, label = "Choose all of interest")),
         column(2, checkboxInput(ns(hVal), value = FALSE, label = "Help Text and Instructions")),
         conditionalPanel(condition = paste0("input['", ns(hVal), "']"),
             column(4, helpText(help))
         )
      )
    )
}

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

#based on suggestion in ???
myConditionalPanel<-function(){
	
}
