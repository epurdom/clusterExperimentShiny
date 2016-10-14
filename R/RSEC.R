RSECHelpText<-function(){ "This is RSEC... more to come"}

rsecOptions<-function(id){
    tagList(
    tabPanel("Main Options",
                      h3("Core imputs for RSEC"),
                      clusterFunctionInputs(id,"clusterInputs",isRSEC=TRUE),
             singleNumericInput(id, sidelabel="Add Proportion Coherence?", aboveLabel="e.g. 0.5", val="combineProportion", defaultValue=0.5,min = 0, max = 1, step = .01 ,help="The proportion of times that two sets of samples should be together in order to be 
                                 grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)", required = FALSE,checkbox=FALSE,functionName="combineMany"),
             singleNumericInput(id, sidelabel="Set minimum size for final cluster?", aboveLabel="e.g. 10", val="combineMinSize", help="minimum size required for a set of samples to be considered in a cluster because of 
                                shared clustering; samples in clusters below this size are unassigned", required = FALSE,checkbox=FALSE,functionName="combineMany")
#              dendroReduce = "mad", dendroNDims = 1000,
#              mergeMethod = "adjP", mergeCutoff = 0.05,
    ),
    tabPanel("Dimensional Reduction",
         #Allows user to enter all inputs
         h3("Choose Dimensionality Reduction Options"),
         dimReduceInput(id, "dim inputs",isRSEC=TRUE)
    ),
    tabPanel("Specialized control",
         specializedInputs(id, "specialized inputs",isRSEC=TRUE)
    )
    )  
}

