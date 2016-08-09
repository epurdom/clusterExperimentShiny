vectorInput<-function(id,sidelabel, aboveLabel,val,aVal, hVal, defaultValue=NULL, help="No help yet available",required=FALSE){
	ns <- NS(id)
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

logicalInput<-function(id,sidelabel, val,aVal, hVal, help="No help yet available",required=FALSE){
	ns<-NS(id)
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

checkboxInput<-function(id, sidelabel,options,val,aVal, hVal, help="No help yet available",required=FALSE){
	ns<-NS(id)
    fluidRow(
      column(3, checkboxInput(ns(aVal), value = FALSE, label = sidelabel)),
      conditionalPanel(condition = paste0("input['", ns(aVal), "']"),
         column(3, checkboxGroupInput(ns(val), choices = options, label = "Choose all of interest")),
         column(2, checkboxInput(ns(hval), value = FALSE, label = "Help Text and Instructions")),
         conditionalPanel(condition = paste0("input['", ns(hval), "']"),
             column(4, helpText(help))
         )
      )
    )
}

#based on suggestion in ???
myConditionalPanel<-function(){
	
}
