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
        setWD(input[["fileInput-workingDirectory"]])
        output$createObjectInputs <- renderUI({
            textInput("objectPath", label = "eg: 'homeDirectory/subdirectory/objectName.rds", 
                      value = input[["fileInput-workingDirectory"]], width = '50%')
        })
    })
    observeEvent(input$makeScript, {
        output$createScriptInputs <- renderUI({
            defaultValue<-if(!is.null(input[["fileInput-workingDirectory"]])) input[["fileInput-workingDirectory"]] else ""
            textInput("filePath", label = "eg: 'homeDirectory/subdirectory/fileName.r", 
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
        #initial comments for start of file 
        cat(paste("\n#Beginning of Shiny script session"), file = filePath, append = TRUE)
        
        cat(paste("\n#Date and time: ", date()), file = filePath, append = TRUE)
        
        cat(paste("\n#", input$fileComments), file = filePath, append = TRUE)
    })
    
    #calling functions from namespaces for uploading files and writing scripts
    rdaFile <- callModule(rdaFile, "fileInput",stringsAsFactors = FALSE)
    datafile <- callModule(dataFile, "fileInput",stringsAsFactors = FALSE)
    csvAssayCode <- callModule(csvAssayCode, "fileInput", stringsAsFactors = FALSE)
    colDataFile <- callModule(colDataFile, "fileInput",stringsAsFactors = FALSE)
    csvColCode <- callModule(csvColCode, "fileInput", stringsAsFactors = FALSE)
    rowDataFile <- callModule(rowDataFile, "fileInput",stringsAsFactors = FALSE)  
    csvRowCode <- callModule(csvRowCode, "fileInput", stringsAsFactors = FALSE)
    
    #This outputs a summary of sE created from the uploaded rda file 
    output$isRda <- renderUI({
        holder <- rdaFile()
        if (is.null(holder))
            return("No data uploaded yet!")
        else {
            #writing code to script
            if(makeFile) {
                cat("\n", 
                    "#loading data:",
                    "sE <- readRDS('InsertFileName')", 
                    sep = "\n",
                    file = filePath, append = TRUE)
            }
            sE <- assignGlobal("sE",holder) 
            #saves object
            if(input$autoCreateObject) {
                saveRDS(sE, input$objectPath)
                if(makeFile) {
                    cat("\n", 
                        "#Save Object:",
                        "saveRDS(sE, '", input$objectPath,
                        "')", 
                        sep = "\n", file = filePath, append = TRUE)
                }
            }
            
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
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        
        output$isAssay <- renderUI({
            assay <- datafile()
            colData <- colDataFile()
            rowData <- rowDataFile()
            #appending script:
            if(makeFile && !is.null(assay)) {
                cat("\n", 
                    "#loading assay data:",
                    "\n",
                    "assay <- ",
                    csvAssayCode(),
                    file = filePath, append = TRUE)
            }
            if(makeFile && !is.null(colData)) {
                cat("\n", 
                    "#loading column data:",
                    "\n",
                    "colData <- ",
                    csvColCode(),
                    file = filePath, append = TRUE)
            }
            if(makeFile && !is.null(rowData)) {
                cat("\n", 
                    "#loading row data:",
                    "\n",
                    "rowData <- ",
                    csvRowCode(),
                    file = filePath, append = TRUE)
            }
            #if statements to add the correct, non-null data frames to sE
            if(!is.null(assay) && !is.null(colData) && !is.null(rowData)) {
                sE <-assignGlobal("sE",SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData),
                                                            rowData = data.matrix(rowData)))
                if(makeFile) {
                    cat("\n", 
                        "#creating sE:",
                        "sE <- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData),
                                    rowData = data.matrix(rowData))",
                        sep = "\n",
                        file = filePath, append = TRUE)
                }
            } else if (!is.null(assay) && !is.null(colData) && is.null(rowData)) {
                sE <-assignGlobal("sE", SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData)))
                #<<- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData))
                if(makeFile) {
                    cat("\n", 
                        "#creating sE:",
                        "sE <- SummarizedExperiment(assays = data.matrix(assay), colData = data.matrix(colData))",
                        sep = "\n",
                        file = filePath, append = TRUE)
                }
            } else if(!is.null(assay) && is.null(colData) && !is.null(rowData)) {
                sE <-assignGlobal("sE",SummarizedExperiment(assays = data.matrix(assay), rowData = data.matrix(rowData))) #<<- SummarizedExperiment(assays = data.matrix(assay), rowData = data.matrix(rowData))
                if(makeFile) {
                    cat("\n", 
                        "#creating sE:",
                        "sE <- SummarizedExperiment(assays = data.matrix(assay), rowData = data.matrix(rowData))",
                        sep = "\n",
                        file = filePath, append = TRUE)
                }
            } else if(!is.null(assay) && is.null(colData) && is.null(rowData)) {
                sE <-assignGlobal("sE", SummarizedExperiment(assays = data.matrix(assay))) #<<- SummarizedExperiment(assays = data.matrix(assay))
                if(makeFile) {
                    cat("\n", 
                        "#creating sE:",
                        "sE <- SummarizedExperiment(assays = data.matrix(assay))",
                        sep = "\n",
                        file = filePath, append = TRUE)
                }
            } else {
                return(HTML(paste("Error, need to upload data file")))
            }
            
            if(input$autoCreateObject) {
                saveRDS(sE, input$objectPath)
                if(makeFile) {
                    cat("\n", 
                        "#Save Object:",
                        "saveRDS(cE, '", input$objectPath,
                        "')", 
                        sep = "\n", file = filePath, append = TRUE)
                }
            }
            return(HTML(paste(capture.output(show(sE)), collapse = "<br/>")))
        })
        
    })
    
    #Outputs to confirm correct uploading of data
    output$csvAssayContents <- renderTable({
        datafile()[1:4, 1:4]
    })
    
    output$csvColContents <- renderTable({
        colDataFile()[1:4, 1:4]
    })
    
    output$csvRowContents <- renderTable({
        rowDataFile()[1:4, 1:4]
    })
    
    
    #---------------End read File inputs-----------------
    
    
    #####################################################
    # Begin Save Object Tab
    #####################################################
    #If clicked, object is saved
    observeEvent(input[["saveObject-createObject"]], {
        objectPath <- input[["saveObject-filePath"]]
        
        output$saveObjectMessage <- renderText({
            if(dim(cE)[1] == 2 && dim(cE)[2] == 1){
                return("No work completed on uploaded object")
            }
            saveRDS(cE, file = objectPath)
            if(makeFile) {
                cat("\n", 
                    "#Save Object:",
                    "saveRDS(cE, '", input$objectPath,
                    "')", 
                    sep = "\n", file = filePath, append = TRUE)
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
        paste(codeList$nIter, " cluster iterations given these choices.")
    })
    
    observeEvent(input$runRSEC, {
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        codeList <- getIterations(codeText=RSECCode(),isRSEC=TRUE,countIterations=FALSE)
        cE<-runCodeAssignGlobal(codeList$fullCodeSE,recordCode=makeFile,recordTag="RSEC:")
        #default plotClusters output from clusterMany
        output$imgCE <-plotClustersServer(RSECPCCode,fileName=NULL,recordCode=makeFile)
    
#         
#         #outfitting proper whichClusters options for futrue widgets
#         output$combineManyWhichClusters <- renderUI({
#             multipleOptionsInput("cMInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
#                                  val = "whichClusters", help = "a numeric or character vector that specifies
#                                  which clusters to compare")
#         })
#         
#         output$makeDendrogramWhichClusters <- renderUI({
#             singleOptionsInput("mDInputs", sidelabel = "Add detailed whichCluster?", options = unique(clusterTypes(cE)),
#                                val = "whichCluster", help = "an integer index or character string that identifies which
#                                cluster should be used to make the dendrogram. Default is primaryCluster.")
#         })
#         
#         output$plotClustersWhichClusters <- renderUI({
#             multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
#                                  val = "whichClusters", help = "an integer index or character string that identifies which
#                                  cluster should be used to plotCluster.")
#         })
#         
#         if(input$autoCreateObject) {
#             saveRDS(cE, input$objectPath)
#             if(makeFile) {
#                 cat("\n", 
#                     "#Save Object:",
#                     "saveRDS(cE, '", input$objectPath,
#                     "')", 
#                     sep = "\n", file = filePath, append = TRUE)
#             }
#         }
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
        paste(codeList$nIter, " cluster iterations given these choices.")
    })
    observeEvent(input$runCM, {
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        codeList <- getIterations(codeText=clusterManyCode(),isRSEC=FALSE,countIterations=FALSE)
        cE<-runCodeAssignGlobal(codeList$fullCodeSE,recordCode=makeFile,recordTag="Cluster Many:")
        #default plotClusters output from clusterMany
        output$imgCE <-plotClustersServer(CMPCCode(),fileName=NULL,recordCode=makeFile)

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
        
        if(input$autoCreateObject) {
            saveRDS(cE, input$objectPath)
            if(makeFile) {
                cat("\n", 
                    "#Save Object:",
                    "saveRDS(cE, '", input$objectPath,
                    "')", 
                    sep = "\n", file = filePath, append = TRUE)
            }
        }
    })
    output$downloadDefaultPlotPCCM <- downloadHandler(
                 filename = "DefaultPlotClustersFromClusterMany.png",
                 content=function(file){plotClustersServer(code= CMPCCode(),fileName=file)}
    )
        
#     output$downloadDefaultPlotPCCM <- downloadHandler(
#         filename = function(){ paste("DefaultPlotFromClusterMany.png")},
#         content = function(file){ 
#             #make sure updated values
#             sE<-get("sE",envir=appGlobal)
#             cE<-get("cE",envir=appGlobal)
#             filePath<-get("filePath",envir=appGlobal)
#             makeFile<-get("makeFile",envir=appGlobal)
#             png(file, height = max((40/3) * nClusters(cE), 480), width = 2*480)
#             defaultMar<-par("mar")
#             plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
#             par(mar=plotCMar)
#             CMPCCode()
#             dev.off()
#         }
#     )
    
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
        cE<-runCodeAssignGlobal(combineManyCode(),recordCode=makeFile,recordTag="Combine Many Code")
        output$imgCombineManyPC <-plotClustersServer(combineManyPCCode(),fileName=NULL,recordCode=makeFile)
        output$imgCombineManyPCC <-plotClustersServer(combineManyCCCode(),fileName=NULL,recordCode=makeFile,type="plotCoClustering")
        
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
        
        if(input$autoCreateObject) {
            saveRDS(cE, input$objectPath)
            if(makeFile) {
                cat("\n", 
                    "#Save Object:",
                    "saveRDS(cE, '", input$objectPath,
                    "')", 
                    sep = "\n", file = filePath, append = TRUE)
            }
        }
        
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
        cE<-runCodeAssignGlobal(makeDendrogramCode(),recordCode=makeFile,recordTag="Make Dendrogram")
        output$imgPlotDendrogram <-plotClustersServer(makeDendrogramPDCode(),fileName=NULL,recordCode=makeFile,type="plotDendrogram")
        output$imgPlotHeatmapMD <-plotClustersServer(makeDendrogramPHCode(),fileName=NULL,recordCode=makeFile,type="plotHeatmap")
        
        
        #**************************************************
        ##This image is output in Merge Clusters Input Page:
        #**************************************************
        output$imgInitalMergeClusters <- renderPlot({
            defaultMar<-par("mar")
            plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            par(mar=plotCMar)
            mergeClusters(cE)
        })
        
        if(input$autoCreateObject) {
            saveRDS(cE, input$objectPath)
            if(makeFile) {
                cat("\n", 
                    "#Save Object:",
                    "saveRDS(cE, '", input$objectPath,
                    "')", 
                    sep = "\n", file = filePath, append = TRUE)
            }
        }
        
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
    
    #merge clusters code
    mergeClustersCode <- callModule(makeMergeClustersCode, "mergeCInputs")
    mergeClustersPCCode <- callModule(makePlotClustersCode, "mergeCInputs",setParameters=FALSE,whichClusters=c("mergeClusters","combineMany","clusterMany")) #function to update code based on users choices.
    mergeClustersPHCode <- callModule(makePlotHeatmapCode, "mergeCInputs",setParameters=FALSE) #function to update code based on users choices.
    
    output$mergeClustersCode <- renderText({
        mergeClustersCode()
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
        cE<-runCodeAssignGlobal(mergeClustersCode(),recordCode=makeFile,recordTag="Merge Clusters based on dendrogram")
        output$imgPlotClustersMergeClusters <-plotClustersServer(mergeClustersPCCode(),fileName=NULL,recordCode=makeFile,type="plotClusters")
        output$imgPlotHeatmapMergeClusters <-plotClustersServer(mergeClustersPHCode(),fileName=NULL,recordCode=makeFile,type="plotHeatmap")
        
        output$plotClustersWhichClusters <- renderUI({
            multipleOptionsInput("pCInputs", sidelabel = "Add detailed whichClusters?", options = unique(clusterTypes(cE)),
                                 val = "whichClusters", help = "an integer index or character string that identifies which
                           cluster should be used to plotCluster.")
        })
        
        if(input$autoCreateObject) {
            saveRDS(cE, input$objectPath)
            if(makeFile) {
                cat("\n", 
                    "#Save Object:",
                    "saveRDS(cE, '", input$objectPath,
                    "')", 
                    sep = "\n", file = filePath, append = TRUE)
            }
        }
        
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
    

    #---------------End merge clusters tab-----------------
    
    #####################################################
    # Start Personalized  plotClusters
    #####################################################
    
    
    plotClustersLatterCode <- callModule(makePlotClustersCode, "pCInputs")
    
    plotClustersCode <- function() {
        code <- paste("plotClusters(cE")
        if(input[["pCInputs-aWhichClusters"]]) {
            code <- paste(code, ", whichClusters = c('",
                          paste(input[["pCInputs-whichClusters"]], collapse = "', '"), "')", sep = "")
        }
        code <- paste(code, plotClustersLatterCode(), sep = "")
        return(code)
    }
    
    output$plotClustersCode <- renderText({
        plotClustersCode()
    })
    
    observeEvent(input$runPCCM, {
        #####
        #make sure updated values
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        
        if(makeFile) {
            cat("\n", 
                "#Specialized call to plotClusters:",
                "#Plotting Clusters",
                "defaultMar<-par('mar')",
                "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
                "par(mar=plotCMar)",
                plotClustersCode(), 
                sep = "\n", file = filePath, append = TRUE)
        }
        
        output$imgPC <- renderPlot({
            defaultMar<-par("mar")
            plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            par(mar=plotCMar)
            eval(parse(text = plotClustersCode()))
        }, 
        #calculating correct height of image:
        height = max((40/3) * sum(clusterTypes(cE) %in% input[["pCInputs-whichClusters"]]), 480))
        
    })
    
    output$downloadSpecializedPlotPCCM <- downloadHandler(
        filename = function(){ paste("specializedPlotFromClusterMany.png")},
        content = function(file){ 
            #####
            #make sure updated values
            sE<-get("sE",envir=appGlobal)
            cE<-get("cE",envir=appGlobal)
            filePath<-get("filePath",envir=appGlobal)
            makeFile<-get("makeFile",envir=appGlobal)
            ######
            png(file, height = max((40/3) * sum(clusterTypes(cE) %in% input[["pCInputs-whichClusters"]]), 480))
            # defaultMar<-par("mar")
            # plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            # par(mar=plotCMar)
            eval(parse(text = plotClustersCode()))
            dev.off()
        }
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
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        
        if(makeFile) {
            cat("\n", 
                "#Specialized call to plotDendrogram:",
                "#Plotting dendrogram",
                "defaultMar<-par('mar')",
                "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
                "par(mar=plotCMar)",
                plotDendrogramCode(), 
                sep = "\n", file = filePath, append = TRUE)
        }
        
        output$imgSpecializedPlotDendrogram <- renderPlot({
            defaultMar<-par("mar")
            plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            par(mar=plotCMar)
            eval(parse(text = plotDendrogramCode()))
        })
    })
    
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
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        
        if(makeFile) {
            cat("\n", 
                "#Specialized call to plotDendrogram:",
                "#Plotting dendrogram",
                "defaultMar<-par('mar')",
                "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
                "par(mar=plotCMar)",
                plotHeatmapCode(), 
                sep = "\n", file = filePath, append = TRUE)
        }
        
        output$imgSpecializedPlotHeatmap <- renderPlot({
            defaultMar<-par("mar")
            plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            par(mar=plotCMar)
            eval(parse(text = plotHeatmapCode()))
        })
    })
    
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
        sE<-get("sE",envir=appGlobal)
        cE<-get("cE",envir=appGlobal)
        filePath<-get("filePath",envir=appGlobal)
        makeFile<-get("makeFile",envir=appGlobal)
        ######
        
        if(makeFile) {
            cat("\n", 
                "#Specialized call to plotDendrogram:",
                "#Plotting dendrogram",
                "defaultMar<-par('mar')",
                "plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)",
                "par(mar=plotCMar)",
                plotCoClusteringCode(), 
                sep = "\n", file = filePath, append = TRUE)
        }
        
        output$imgSpecializedPlotCoClustering <- renderPlot({
            defaultMar<-par("mar")
            plotCMar<-c(.25 * 1.1, 3 * 8.1, .25 * 4.1, 3 * 1.1)
            par(mar=plotCMar)
            eval(parse(text = plotCoClusteringCode()))
        })
    })
    
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





