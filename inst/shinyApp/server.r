#This line allows large file uploads
options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output, session) {
    #make sure updated values
    sE<-get("sE",envir=appGlobal)
    cE<-get("cE",envir=appGlobal)
    filePath<-get("filePath",envir=appGlobal)
    makeFile<-get("makeFile",envir=appGlobal)
    #####################################################
    # Begin read file outputs
    #####################################################
    countModule<-callModule(getIsCount,"trans")
    
    #When the create workign directory button is clicked, WD is set and the defalt inputs for the save
    #script and save object widgets are created
    observeEvent(input$createWD, {
        setwd(input[["workingDirectory"]])
    })
    
    observeEvent(input$autoCreateObject, {
        output$createObjectInputs <- renderUI({
            defaultValue<-if(!is.null(input[["workingDirectory"]])) input[["workingDirectory"]] else ""
            textInput("objectPath", label = "eg: 'homeDirectory/subdirectory/objectName.rds", 
                      value = defaultValue, width = '50%')
        })
    })
    
    observeEvent(input$makeScript, {
        output$createScriptInputs <- renderUI({
            defaultValue<-if(!is.null(input[["workingDirectory"]])) input[["workingDirectory"]] else ""
            textInput("filePath", label = "eg: homeDirectory/subdirectory/fileName.r", 
                      value = defaultValue, width = '100%')
        })
    })
    #creates reproducible file if it doens't already exist when clicked
    observeEvent(input$createReproducibleFile, {
        makeFile<-assignGlobal("makeFile",value=TRUE) 
        filePath<-assignGlobal("filePath",value=input$filePath) 
        if(!file.exists(filePath)) {
            file.create(filePath)
        }
        append<-TRUE #maybe should add option?
        #initial comments for start of file 
        cat("\n##########\n#Beginning of Shiny script session\n########", file = filePath, append = append)
        cat(paste("\n#Date and time: ", date()), file = filePath, append = TRUE)
        cat(paste("\n#", input$fileComments), file = filePath, append = TRUE)
        if(!is.null(input[["workingDirectory"]])){
            cat(sprintf("\nsetwd(%s)",input[["workingDirectory"]]), file = filePath, append = TRUE)
        }
        cat(paste("\nrequire(clusterExperiment)"), file = filePath, append = TRUE)
        
    })
    
    #calling functions from namespaces for uploading files and writing scripts
    #This outputs a summary of sE created from the uploaded rds file and saves it as sE
    rdaFile <- callModule(rdaFile, "fileInput",stringsAsFactors = FALSE,recordCode=get("makeFile",envir=appGlobal),recordFile=get("filePath",envir=appGlobal))
    output$isRda <- renderUI({
        holder <- rdaFile()
        if (is.null(holder))
            return("No data uploaded yet!")
        else { #could move this to the rdaFile() command... but probably complicated...
            #writing code to script
            sE <- assignGlobal("sE",holder) 
            #saves object
            if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("sE"),recordCode=get("makeFile",envir=appGlobal))
            #Creating which clusters options in various later tabs, only if object is type cluster experiment
            if(class(sE)[1] == "ClusterExperiment") {
                cE<-assignGlobal("cE",sE) 
                #combine many
                output$combineManyWhichClusters <- renderUI({
                    multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?",
                                         options = unique(clusterTypes(cE)), val = "whichClusters",functionName="combineMany",
                                         help = "a numeric or character vector that specifies which clusters 
                               to compare")
                })
                #make dendrogram
                output$makeDendrogramWhichClusters <- renderUI({
                    singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", 
                                       options = unique(clusterTypes(cE)), val = "whichCluster", functionName="makeDendrogram",
                                       help = "an integer index or character string that identifies which
                             cluster should be used to make the dendrogram. Default is primaryCluster.")
                })
                
                #plot clusters
                output$plotClustersWhichClusters <- renderUI({
                    multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", 
                                         options = unique(clusterTypes(cE)), val = "whichClusters", functionName="plotClusters",
                                         help = "an integer index or character string that identifies which
                               cluster should be used to plotCluster.")
                })
            }
            #the summary that is being returned
            HTML(
                paste(capture.output(show(sE)), collapse = "<br/>")
            )
        }
    })
    
    #creating Summarized Experiment object from uploaded .csv data
    observeEvent(input$makeObject, {
        #####
        #make sure updated values
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        output$isAssay <- renderUI({
            assay<-callModule(readCSVInput,"fileInput", recordCode=get("makeFile",envir=appGlobal),recordFile=filePath,whichData="file",recordSection=TRUE)()
            colData <- callModule(readCSVInput,"fileInput", recordCode=get("makeFile",envir=appGlobal),recordFile=filePath,whichData="colData")()
            rowData <- callModule(readCSVInput,"fileInput", recordCode=get("makeFile",envir=appGlobal),recordFile=filePath,whichData="rowData")()
            sE<-createSummarizedExp(assay,colData,rowData,recordCode=get("makeFile",envir=appGlobal),recordFile=filePath,recordTag="Combine input data into SummarizedExperiment")
            assignGlobal("sE",value=sE)
            if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("cE"),recordCode=get("makeFile",envir=appGlobal))
            return(HTML(paste(capture.output(show(sE)), collapse = "<br/>")))
        })
    })
    
    #Outputs to confirm correct uploading of data
    output$csvAssayContents <- renderTable({
        callModule(readCSVInput,"fileInput", recordCode=FALSE,whichData="file",nrows=4)()[, 1:4]
    })
    output$csvColContents <- renderTable({
        callModule(readCSVInput,"fileInput", recordCode=FALSE,whichData="colData",nrows=4)()[, 1:4]
    })
    output$csvRowContents <- renderTable({
        callModule(readCSVInput,"fileInput", recordCode=FALSE,whichData="rowData",nrows=4)()[, 1:4]
    })
    #---------------End read File inputs-----------------
    
    
    #####################################################
    # Begin Save Object Tab
    #####################################################
    #If clicked, object is saved
    observeEvent(input[["saveObject-createObject"]], {
        ##########
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ##########
        objectPath <- input[["saveObject-filePath"]]
        
        output$saveObjectMessage <- renderText({
            if(dim(cE)[1] == 2 && dim(cE)[2] == 1){
                return("No work completed on uploaded object")
            }
            saveRDS(cE, file = objectPath)
            if(makeFile) {
                code<-paste("saveRDS(cE, '", input$objectPath,"')",sep="")
                recordCodeFun(code=code,tag="Save Object")
            }
            return(paste("successfully saved internal clusterExperiment object in ", 
                         objectPath, " via function saveRDS()"))
        })
        
    })
    
    #---------------End save Object tab-----------------
    #####################################################
    # Begin RSEC Many Tab
    #####################################################
    #Calling modular functions
    RSECCode <- callModule(makeClusterManyCode, "rsec",isRSEC=TRUE,countModule=countModule) #function to update code based on users choices.
    RSECPCCode <- callModule(makePlotClustersCode, "rsec",setParameters=FALSE,whichClusters="workflow") #function to update plotClusters (not really update, because doesn't change)
    
    #reactive code to be run internally
    #can these two be combined??
    output$RSECCode <- renderText({
        codeList <- getIterations(codeText=RSECCode(),isRSEC=TRUE,countIterations=FALSE)
        codeList$fullCodeSE
    })
    output$numRSECIterations <- renderText({
        #codeToBeEvaluated <- paste("clusterMany(sE, run = FALSE ", clusterManyCode(),")", sep = "")
        codeList <- getIterations(codeText=RSECCode(),isRSEC=TRUE)
        codeList$nIter
    })
    
    observeEvent(input$runRSEC, {
        ##########
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ##########
        codeList <- getIterations(codeText=RSECCode(),isRSEC=TRUE,countIterations=FALSE)
        cE<-runCodeAssignGlobal(codeList$fullCodeSE,recordCode=get("makeFile",envir=appGlobal),recordTag="RSEC:")
        #default plotClusters output from clusterMany
        output$imgCE <-plotClustersServer(RSECPCCode,fileName=NULL,recordCode=get("makeFile",envir=appGlobal))
    
        
        #outfitting proper whichClusters options for futrue widgets
        output$combineManyWhichClusters <- renderUI({
            multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", help = "a numeric or character vector that specifies
                                 which clusters to compare")
        })
        
        output$makeDendrogramWhichClusters <- renderUI({
            singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", options = unique(clusterTypes(cE)),
                               val = "whichCluster", help = "an integer index or character string that identifies which
                               cluster should be used to make the dendrogram. Default is primaryCluster.")
        })
        
        output$plotClustersWhichClusters <- renderUI({
            multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", help = "an integer index or character string that identifies which
                                 cluster should be used to plotCluster.")
        })
         if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("cE"),recordCode=get("makeFile",envir=appGlobal))

         })
#     
#default plotClusters output from RSEC

    output$downloadDefaultPCRSEC <- downloadHandler(
        filename = "DefaultPlotClustersFromRSEC.png",
        content=function(file){plotClustersServer(code= RSECPCCode(),fileName=file,recordCode=FALSE)}
    )

    
    #---------------End RSEC tab-----------------
    
    #####################################################
    # Begin Cluster Many Tab
    #####################################################
    #Calling modular functions
    clusterManyCode <- callModule(makeClusterManyCode, "parameters",countModule=countModule)
    CMPCCode <- callModule(makePlotClustersCode, "parameters",setParameters=FALSE,whichClusters="clusterMany") #function to update code based on users choices.
    
    #reactive code to be run internally
    #can these two be combined??
    output$clusterManyCode <- renderText({
        codeList <- getIterations(codeText=clusterManyCode(),isRSEC=FALSE,countIterations=FALSE)
        codeList$fullCodeSE
        })
    output$numClusterIterations <- renderText({
        codeList <- getIterations(codeText=clusterManyCode(),isRSEC=FALSE)
        codeList$nIter
    })
    observeEvent(input$runCM, {
        ##########
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ##########
        codeList <- getIterations(codeText=clusterManyCode(),isRSEC=FALSE,countIterations=FALSE)
        cE<-runCodeAssignGlobal(codeList$fullCodeSE,recordCode=get("makeFile",envir=appGlobal),recordTag="Cluster Many")
        #default plotClusters output from clusterMany
        output$imgCE <-plotClustersServer(CMPCCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal))

        #outfitting proper whichClusters options for future widgets
        output$combineManyWhichClusters <- renderUI({
            multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", functionName="combineMany",help = "a numeric or character vector that specifies
                             which clusters to compare")
        })
        
        output$makeDendrogramWhichClusters <- renderUI({
            singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", options = unique(clusterTypes(cE)),
                               val = "whichCluster", functionName="makeDendrogram",help = "an integer index or character string that identifies which
                             cluster should be used to make the dendrogram. Default is primaryCluster.")
        })
        
        output$plotClustersWhichClusters <- renderUI({
            multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", functionName="plotClusters",help = "an integer index or character string that identifies which
                           cluster should be used to plotCluster.")
        })
        if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("cE"),recordCode=get("makeFile",envir=appGlobal))
        
        
    })
    output$downloadDefaultPlotPCCM <- downloadHandler(
                 filename = "DefaultPlotClustersFromClusterMany.png",
                 content=function(file){plotClustersServer(code= CMPCCode(),fileName=file)}
    )

    #---------------End cluster Many tab-----------------
    
    #####################################################
    # Start Combine Many Tab
    #####################################################
    
    #renders the code for the second half of combine many
    combineManyCode <- callModule(makeCombineManyCode, "cMInputs")
    combineManyPCCode <- callModule(makePlotClustersCode, "cMInputs",setParameters=FALSE,whichClusters=c("combineMany","clusterMany")) #function to update code based on users choices.
    combineManyCCCode <-callModule(makePlotCoClusteringCode,"cMInputs",setParameters=FALSE) #function to update code based on users choices.
    output$combineManyCode <- renderText({combineManyCode()})
    #run combine many button
    observeEvent(input$runCombineMany, {
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        cE<-runCodeAssignGlobal(combineManyCode(),recordCode=get("makeFile",envir=appGlobal),recordTag="Combine Many Code")
        output$imgCombineManyPC <-plotClustersServer(combineManyPCCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal))
        output$imgCombineManyPCC <-plotClustersServer(combineManyCCCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotCoClustering")
        
#         output$makeDendrogramWhichClusters <- renderUI({
#             singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", options = unique(clusterTypes(cE)),
#                                val = "whichCluster", help = "an integer index or character string that identifies which
#                            cluster should be used to make the dendrogram. Default is primaryCluster.")
#         })
        
        output$plotClustersWhichClusters <- renderUI({
            multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", help = "an integer index or character string that identifies which
                         cluster should be used to plotCluster.")
        })
        if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("cE"),recordCode=get("makeFile",envir=appGlobal))
        
       
        
    })
    
    ###Save the default plots at user's request
    output$downloadDefaultPlotPCCM <- downloadHandler(
        filename = "defaultPlotClusterFromCombineMany.png",
        content=function(file){plotClustersServer(code= combineManyPCCode(),fileName=file)}
    )
    output$downloadDefaultPlotCoClustersCombineMany <- downloadHandler(
        filename = "defaultCoClusterFromCombineMany.png",
        content=function(file){plotClustersServer(code= combineManyCCCode(),fileName=file,type="plotCoClustering")}
    )

    #---------------End combine Many tab-----------------
    
    #####################################################
    # Start make Dendrogram Tab
    #####################################################
    
    #makes the second half of dendrogram code
    makeDendrogramCode <- callModule(makeMakeDendrogramCode, "mDInputs")
    makeDendrogramPDCode <- callModule(makePlotDendrogramCode, "mDInputs",setParameters=FALSE) #function to update code based on users choices.
    makeDendrogramPHCode <- callModule(makePlotHeatmapCode, "mDInputs",setParameters=FALSE) #function to update code based on users choices.

    #merge clusters code -- put here so available to
    mergeClustersPDCode <- callModule(makeMergeClustersCode, "mergeCInputs",plot=TRUE)

    output$makeDendrogramCode <- renderText({
        makeDendrogramCode()
    })
    #When clicked runs displayed make dendrogram code
    observeEvent(input$runMakeDendrogram, {
        #####
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        cE<-runCodeAssignGlobal(makeDendrogramCode(),recordCode=get("makeFile",envir=appGlobal),recordTag="Make Dendrogram")
        output$imgPlotDendrogram <-plotClustersServer(makeDendrogramPDCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotDendrogram")
        output$imgPlotHeatmapMD <-plotClustersServer(makeDendrogramPHCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotHeatmap")
        
        
        #**************************************************
        ##This image is output in Merge Clusters Input Page (do here, once dendrogram created):
        #**************************************************
        output$imgInitalMergeClusters <- plotClustersServer("mergeClusters(cE,mergeMethod='none',plotType='all')",fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="mergeClusters")
        if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("cE"),recordCode=get("makeFile",envir=appGlobal))
        
        
    })
    ###Save the default plots at user's request
    output$downloadDefaultPlotPDMD <- downloadHandler(
        filename = "defaultPlotDendrogramFromMakeDendrogram.png",
        content=function(file){plotClustersServer(code= makeDendrogramPDCode(),fileName=file,type="plotDendrogram")}
    )
    output$downloadDefaultPlotPHMD <- downloadHandler(
        filename = "defaultHeatmapFromMakeDendrogram.png",
        content=function(file){plotClustersServer(code= makeDendrogramPHCode(),fileName=file,type="plotHeatmap")}
    )

    
    #---------------End make dendrogram tabe-----------------
    
    #####################################################
    # Start mergeClusters
    #####################################################
    mergeClustersCode <- callModule(makeMergeClustersCode, "mergeCInputs")
    mergeClustersPCCode <- callModule(makePlotClustersCode, "mergeCInputs",setParameters=FALSE,whichClusters=c("mergeClusters","combineMany","clusterMany")) #function to update code based on users choices.
    mergeClustersPHCode <- callModule(makePlotHeatmapCode, "mergeCInputs",setParameters=FALSE) #function to update code based on users choices.
    
    
    output$mergeClustersCode <- renderText({
        mergeClustersCode()
    })
    
    mergeClustersPDCode <- callModule(makeMergeClustersCode, "mergeCInputs",plot=TRUE)
    #When clicked updates dendrogram
    observeEvent(input$updateDendrogram, {
        output$imgInitalMergeClusters <- plotClustersServer(mergeClustersPDCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="mergeClusters")
    })
    #button runs merge clusters code
    observeEvent(input$runMergeClusters, {
        #####
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        cE<-runCodeAssignGlobal(mergeClustersCode(),recordCode=get("makeFile",envir=appGlobal),recordTag="Merge Clusters based on dendrogram")
        output$imgPlotClustersMergeClusters <-plotClustersServer(mergeClustersPCCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotClusters")
        output$imgPlotHeatmapMergeClusters <-plotClustersServer(mergeClustersPHCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotHeatmap")
        
        output$plotClustersWhichClusters <- renderUI({
            multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", help = "an integer index or character string that identifies which
                           cluster should be used to plotCluster.")
        })
        
        if(input$autoCreateObject) saveObjects(path=input$objectPath,type=c("cE"),recordCode=get("makeFile",envir=appGlobal))
        
        
    })
    ###Save the default plots at user's request
    output$downloadDefaultPlotClustersMergeClusters <- downloadHandler(
        filename = "defaultPlotClustersFromMergeClusters.png",
        content=function(file){plotClustersServer(code= mergeClustersPCCode(),fileName=file,type="plotClusters")}
    )
    output$downloadDefaultPlotHeatmapMergeClusters <- downloadHandler(
        filename = "defaultHeatmapFromMergeClusters.png",
        content=function(file){plotClustersServer(code= mergeClustersPHCode(),fileName=file,type="plotHeatmap")}
    )
    output$downloadPlotPDMC<-downloadHandler(
        filename = "defaultDendrogramFromMergeClusters.png",
        content=function(file){plotClustersServer(code= mergeClustersPDCode(),fileName=file,type="mergeClusters")}
    )

    #---------------End merge clusters tab-----------------
    
    #####################################################
    # Start Personalized  plotClusters
    #####################################################
    plotClustersCode <- callModule(makePlotClustersCode, "pCInputs")

    output$plotClustersCode <- renderText({
        plotClustersCode()
    })
    
    observeEvent(input$runPCCM, {
        #make sure updated values
        makeFile<-get("makeFile",envir=appGlobal)
        output$imgPC <- plotClustersServer(plotClustersCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotClusters")
    })
    
    output$downloadSpecializedPlotPCCM <- downloadHandler(
        filename = function(){ paste("specializedPlotClustersPlot.png")},
        content = function(file){plotClustersServer(code= plotClustersCode(),fileName=file,type="plotClusters")}
    )
    
    #---------------End personalized plot clusters tab-----------------
    
    
    #####################################################
    # Start Personalized  plotDendrogram
    #####################################################
    plotDendrogramCode <- callModule(makePlotDendrogramCode, "plotDendrogram")
    
    output$plotDendrogramCode <- renderText({
        plotDendrogramCode()
    })
    
    observeEvent(input$runPlotDendrogram, {
        #####
        #make sure updated values
        makeFile<-get("makeFile",envir=appGlobal)
        output$imgSpecializedPlotDendrogram <- plotClustersServer(plotDendrogramCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotDendrogram")
    })
    output$downloadSpecializedPlotDendrogram <- downloadHandler(
        filename = function(){ paste("specializedDendrogramPlot.png")},
        content = function(file){plotClustersServer(code= plotDendrogramCode(),fileName=file,type="plotDendrogram")}
    )
    #---------------End peronalized plot Dendrogram tab-----------------
    
    
    #####################################################
    # Start Personalized  plotHeatmap
    #####################################################
    
    plotHeatmapCode <- callModule(makePlotHeatmapCode, "plotHeatmap")
    
    output$plotHeatmapCode <- renderText({
        plotHeatmapCode()
    })
    
    observeEvent(input$runPlotHeatmap, {
        #####
        #make sure updated values
        makeFile<-get("makeFile",envir=appGlobal)
        output$imgSpecializedPlotHeatmap <- plotClustersServer(plotHeatmapCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotHeatmap")
    })
    output$downloadSpecializedPlotHeatmap <- downloadHandler(
        filename = function(){ paste("specializedHeatmapPlot.png")},
        content = function(file){plotClustersServer(code= plotHeatmapCode(),fileName=file,type="plotHeatmap")}
    )
    
    #---------------End personalized plot heatmap tab-----------------
    
    #####################################################
    # Start Personalized  plotCoClustering
    #####################################################
    
    plotCoClusteringCode <- callModule(makePlotCoClusteringCode, "plotCoClustering")
    
    output$plotCoClusteringCode <- renderText({
        plotCoClusteringCode()
    })
    
    observeEvent(input$runPlotCoClustering, {
        #####
        #make sure updated values
        makeFile<-get("makeFile",envir=appGlobal)
        output$imgSpecializedPlotCoClustering <- plotClustersServer(plotCoClusteringCode(),fileName=NULL,recordCode=get("makeFile",envir=appGlobal),type="plotCoClustering")

        output$imgSpecializedPlotCoClustering <- renderPlot({
            defaultMar<-par("mar")
            plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            par(mar=plotCMar)
            eval(parse(text = plotCoClusteringCode()))
        })
    })
    output$downloadSpecializedPlotCoClustering <- downloadHandler(
        filename = function(){ paste("specializedCoClusteringPlot.png")},
        content = function(file){plotClustersServer(code= plotCoClusteringCode(),fileName=file,type="plotCoClustering")}
    )
    #---------------End personalized plot CoClustering tab-----------------
    
    
    #####################################################
    # Start What Clusters
    #####################################################
    
    
    observeEvent(input$showSummmary, {
        #####
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        
        output$cESummary <- renderTable({
            if(dim(cE)[1] == 2 && dim(cE)[2] == 1 && dim(sE)[1] == 0 && dim(sE)[2] == 2){
                return("No uploaded object")
            } else if(dim(cE)[1] == 2 && dim(cE)[2] == 1) {
                return(data.frame(c("No work done on object", 
                                    "run clusterMany to create a cluster experiment", 
                                    "Or upload a cluster experiment object"), fix.empty.names = FALSE))
            } else {
                return(workflowClusterDetails(cE))
            }
            
        })
    })
    #---------------End What Clusters tab-----------------
    
})





