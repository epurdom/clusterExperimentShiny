RSECHelpText<-function(){ "This is RSEC... more to come"}

RSECInputs<-function(id){
    ns <- NS(id)
    tagList(
        clusterFunctionInputs("rsec","clusterInputs",isRSEC=TRUE),
        singleNumericInput(id=id,sidelabel="Set proportion shared (combining clusters)?",aboveLabel="e.g. 0.7", val="combineProportion", min = 0, max = 1, step = .1, help="The proportion of times that two sets of samples should be together in order to be 
                                 grouped into a cluster (if <1, passed to clusterD via alpha = 1 - proportion)" , functionName="RSEC"),
        singleNumericInput(id=id,sidelabel="Set minimum size (combining clusters)?",aboveLabel="e.g. 10", val="combineMinSize",help="minimum size required for a set of samples to be considered in a cluster because of shared clustering" , functionName="RSEC")
    )
}