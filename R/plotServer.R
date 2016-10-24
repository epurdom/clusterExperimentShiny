#' @rdname InternalModules
#' @export
#if fileName=NULL, renders plot; otherwise saves as png based on fileName
plotClustersServer<-function(code,fileName=NULL,recordCode=FALSE,
                             type=c("plotClusters","plotCoClustering","plotHeatmap","plotDendrogram","mergeClusters")){
    type<-match.arg(type)
    cE<-get("cE",envir=appGlobal)
    #if(type=="plotDendrogram") browser()
    tmp<-strsplit(code,"whichClusters=") #assumes whichClusters is always last in plotCluster command
    if(length(tmp[[1]])>1){
        whichClusters<-gsub("c(","",tmp[[1]][2])
        whichClusters<-gsub(")","",whichClusters)
        whichClusters<-strsplit(whichClusters,",")[[1]]
        nclustersPlot<-sum(clusterTypes(cE) %in% whichCusters)
    }
    else{ #use default which is "workflow"
        nclustersPlot<-ncol(workflowClusters(cE))
    }
    heightR<-if(type=="plotClusters") max((40/3) * nclustersPlot, 480) else "auto"
    heightFixed<-if(type=="plotClusters") heightR else 480
    widthFixed<-if(type=="plotCluster")2*480 else 480
#if want to specialize by type plot, should go back to switch:
    #     heightR<-switch(type,"plotCluster"= max((40/3) * nclustersPlot, 480),
#                    "plotCoClustering"="auto")
#     heightFixed<-switch(type,"plotCluster"= heightR,
#                     "plotCoClustering"=480)
#     widthFixed<-switch(type,"plotCluster"=2*480,"plotCoClustering"=480)
    plotCMar<-if(type=="plotClusters") c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1) else c(5.1 ,4.1 ,4.1 ,2.1)
    
    if(is.null(fileName)){ #not a download code
        if(recordCode){
            recordTag<-switch(type,
                             "plotClusters"="Plot Clusters", 
                             "plotCoClustering"="Plot Co-Clustering",
                             "plotHeatmap"="Plot Heatmap",
                             "plotDendrogram"="Plot Dendrogram",
                             "mergeClusters"="Dendrogram from MergeClusters Plot")
            recordCodeFun(code,tag=recordTag)
        }
        #if(type=="mergeClusters") browser()
        renderPlot({
            par(mar=plotCMar)
            eval(parse(text = code))
        }, height = heightR)
    } 
    else{
        png(fileName, height = heightFixed, width = widthFixed)
        par(mar=plotCMar)
        eval(parse(text = code))
        dev.off()
    }
}