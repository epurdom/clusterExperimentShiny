options(getClass.msg=FALSE) 
appGlobal<-new.env()
assign("startingDirectory",value= getwd(),envir=appGlobal)
assign("sE",value= SummarizedExperiment(),envir=appGlobal)
assign("cE",value= clusterExperiment(matrix(data = c(0,0)), clusters = c(0), transformation = function(x){x}),envir=appGlobal)
assign("makeFile",value= FALSE,envir=appGlobal)
assign("filePath",value= NULL,envir=appGlobal)   ## dummy example 
