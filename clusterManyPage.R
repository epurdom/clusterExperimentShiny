#This file, clusterManyPage.R, contains the functions, inputs, and server-side computations of the clusterMany tab.

library(stringr)

#This userFile input is a very large function that recieves all of the inputs for clusterMany from the user

dimReduceInput <- function(id, label = "inputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(

    fluidRow(
      column (4, 
              h3("Dimension Reduction:"),
              helpText("Choose what types of dimensionality reduction to perform before clustering. "),
              checkboxGroupInput(ns("dimReduce"), choices = c("none","PCA", "var","cv", "mad"), 
                          label = "Dimensionality Reduction Method:", selected = "none")
      ),
      column (4, 
              #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
              conditionalPanel(condition = paste0("input['", ns("dimReduce"), "'][0] == 'PCA'",
                                                  "|| input['", ns("dimReduce"), "'][1] == 'PCA'",
                                                  "|| input['", ns("dimReduce"), "'][2] == 'PCA'",
                                                  "|| input['", ns("dimReduce"), "'][3] == 'PCA'",
                                                  "|| input['", ns("dimReduce"), "'][4] == 'PCA'"),
                               h3("Number of Principle Components to retain:"),
                               helpText("please enter a list (separated by commas) of the number of PCs to use
                                        (when 'PCA' is identified in dimReduce).
                                        If NA is included, then the full dataset will also be included."),
                               textInput(ns("nPCADims"),
                                            label = "Number of PCs to retain",
                                            value = 1)
              )
      ),
      #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
      column(4,
             conditionalPanel(condition = paste0("input['", ns("dimReduce"), "'][0] == 'mad'",
                                                 "|| input['", ns("dimReduce"), "'][1] == 'mad'",
                                                 "|| input['", ns("dimReduce"), "'][2] == 'mad'",
                                                 "|| input['", ns("dimReduce"), "'][3] == 'mad'",
                                                 "|| input['", ns("dimReduce"), "'][4] == 'mad'",
                                                 "|| input['", ns("dimReduce"), "'][0] == 'var'",
                                                 "|| input['", ns("dimReduce"), "'][1] == 'var'",
                                                 "|| input['", ns("dimReduce"), "'][2] == 'var'",
                                                 "|| input['", ns("dimReduce"), "'][0] == 'cv'",
                                                 "|| input['", ns("dimReduce"), "'][1] == 'cv'",
                                                 "|| input['", ns("dimReduce"), "'][2] == 'cv'",
                                                 "|| input['", ns("dimReduce"), "'][3] == 'cv'"),
                               h3("How many variables the dimensionality reduction should keep :"),
                               helpText("Please enter a list (separated by commas) of the number of the most variable
                                        features to keep (when 'var', 'cv', or 'mad' is identified in dimReduce).
                                        If NA is included, then the full dataset will also be included."),
                               textInput(ns("nVarDims"),
                                            label = "Number of dimensions to retain",
                                            value = 1)
              )
    )
    )
  )
}

clusterFunctionInputs <- function(id, label = "inputs") {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      #cluster Function Box, not conditional
      column(3, 
             h3("Cluster Function:"),
             helpText("Function used for the clustering."),
             checkboxGroupInput(ns("clusterFunction"), choices = c("tight", "hierarchical01","hierarchicalK", "pam"), 
                         label = "Function for Clustering:", selected = "tight")
      ),
      #input alpha, conditional on clusterFunction = "tight" or clusterFunction = "hierarchical01"
      #Danger!
      #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
      column(6,
             conditionalPanel(condition = paste0("input['", ns("clusterFunction"), "'][0] == 'tight'", 
                                                 " || input['", ns("clusterFunction"), "'][0] == 'hierarchical01' ",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'tight' ",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'hierarchical01' ",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'tight' ",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'hierarchical01' ",
                                                 " || input['", ns("clusterFunction"), "'][3] == 'tight' ",
                                                 " || input['", ns("clusterFunction"), "'][3] == 'hierarchical01' "),             
                              h3("Alphas:"),
                              helpText("Values of alpha to be tried. 
                                       Determines tightness required in creating clusters from the dissimilarity matrix.
                                       In this box you are simply choosing the number of alphas to be tried. 
                                       Please enter numbers between 0.0 and 1.0, comma delimited." ),
                              textInput(ns("alphas"),
                                        label = "alpha vector",
                                        value = "0.25, 0.74")
                              )
             #find best K logical, conditional on clusterFunction = "hierarchicalK"
      )
    ),
    fluidRow(
      column(3,
             conditionalPanel(condition = paste0("input['", ns("clusterFunction"), "'][0] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][3] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][0] == 'pam'",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'pam'",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'pam'",                                              
                                                 " || input['", ns("clusterFunction"), "'][3] == 'pam'"),             
                              h3("Find Best K?"),
                              helpText("Whether should find best K based on average silhouette width
                                       (only used if clusterFunction of type 'K')"),
                              checkboxGroupInput(ns("findBestK"), label = NULL, choices = c("TRUE", "FALSE"),
                                                 selected = "FALSE")
            )
      ),
      
      # remove Sil logical, conditional upon clusterFunction = "hierarchicalK"
      column(3,
             conditionalPanel(condition = paste0("input['", ns("clusterFunction"), "'][0] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][3] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][0] == 'pam'",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'pam'",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'pam'",                                              
                                                 " || input['", ns("clusterFunction"), "'][3] == 'pam'"),           
                              h3("Remove Silhouette?"),
                              helpText("logical as to whether remove when silhouette < silCutoff
                                       (only used if clusterFunction of type 'K'')"),
                              checkboxGroupInput(ns("removeSil"), label = NULL, choices = c("TRUE", "FALSE"),
                                                 selected = "FALSE")                                       
              )
      ),
      
      #Enter Sil cutoff, conditional upon removeSil == TRUE,  which is conditional upon clusterFunction = "hierarchicalK"
      column(3,
             conditionalPanel(condition = paste0("(input['", ns("clusterFunction"), "'][0] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][3] == 'hierarchicalK'",
                                                 " || input['", ns("clusterFunction"), "'][0] == 'pam'",
                                                 " || input['", ns("clusterFunction"), "'][1] == 'pam'",
                                                 " || input['", ns("clusterFunction"), "'][2] == 'pam'",                                              
                                                 " || input['", ns("clusterFunction"), "'][3] == 'pam')",
                                                 " && input['", ns("removeSil"), "'][0] == 'TRUE'"),             
                              h3("Silhouette Cutoff"),
                              helpText("Requirement on minimum silhouette width to be included in cluster 
                                       (only if removeSil=TRUE). 
                                       Please enter a list (separated by commas) of the number of the"),
                              textInput(ns("silCutoff"), label = NULL)
              )
      
    )
),
    
    
    fluidRow(
      # I need some more technical knowhow to feel confident in my ks design
      # K values, not conditional
      column(12, 
             h3("K Values"),
             helpText("The argument 'ks' is interpreted differently for different choices of the other parameters. 
                      When/if sequential=TRUE, ks defines the argument k0 of seqCluster. 
                      Otherwise, 'ks' values are set in both subsampleArgs[['k']] and clusterDArgs[['k']]
                      that are passed to clusterD and subsampleClustering. 
                      This passing of these arguments via subsampleArgs[['k']] will only have an effect if 'subsample=TRUE'.
                      Similarly, the passing of clusterDArgs[['k']] will only have an effect when the clusterFunction 
                      argument includes a clustering algorithm of type 'K'. When/if 'findBestK=TRUE',
                      ks also defines the kRange argument of clusterD unless kRange is specified by the user via the 
                      clusterDArgs; note this means that the default option of setting kRange that depends on the input k 
                      (see clusterD) is not available in clusterMany."),
             sliderInput(ns("ks"), label = "Range of K Values:", min = 1, max = 100, value = c(3,6),
                         step = 1, ticks = TRUE, round = TRUE)
             )
    ),
    
    fluidRow(
      
      column(3,
             h3("distFunction"),
             helpText("need clarity")
      ),
      
      
      column (3, 
              h3("Transform Function:"),
              helpText("Help")
              # *Input(),
              # *Output("")
      ), 
      
      # #This might need to be down with clusterD by line 27X
      # #Enter Min clustr Sizes, not conditional
      column(3,
             h3("Minimum Cluster Sizes"),
             helpText("the minimimum size required for a cluster (in clusterD).
                      Clusters smaller than this are not kept and samples are left unassigned.
                      Minimum cluster Size is 2. Please enter numbers separeted by commas"),
             textInput(ns("minSizes"), label = NULL, value = 5),
             tabPanel("minSizesCode", verbatimTextOutput("minSizeCode"))
      )
      )
  )
}

sSBInputs <- function(id, label = "SSB inputs") {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      # I need some more technical knowhow to feel confident in my Sequential design, 
      #I think this may be reduntant visually, perhaps this should be handled internally
      # Logical Sequential, not conditional
      column(3,
             h3("Sequential"),
             helpText("logical whether to use the sequential strategy"),
             checkboxInput(ns("sequential"), label = NULL, value = FALSE)
      ),
      
      #Enter Betas, conditional upon sequential == TRUE
      #Danger!
      column(6,
             conditionalPanel(condition = paste0("input['", ns("sequential"), "']"),             
                              h3("Betas:"),
                              helpText("values of beta to be tried in sequential steps. 
                                       Only used for sequential=TRUE. 
                                       Determines the similarity between two clusters required in order to deem 
                                       the cluster stable. Takes on values in [0,1]." ),
                              textInput(ns("betas"),
                                        label = "beta vector",
                                        value = NULL)
                              )
      ),
      #Logical subsample, not conditional 
      column(3,
             h3("Subsample"),
             helpText("logical as to whether to subsample via subsampleClustering to get the distance matrix 
                      at each iteration; otherwise the distance function will be determined by argument 
                      distFunction passed in clusterDArgs."),
             checkboxGroupInput(ns("subsample"), label = NULL, choices = c("TRUE", "FALSE"), selected = "FALSE")
             )
      
    ),
    
    fluidRow(
      #Enter number of cores, not conditional
      column(3,
             h3("Number of Cores"),
             helpText("The number of threads"),
             numericInput(ns("ncores"), label = NULL, value = 1, min = 1, step = 1)
      ),
      
      #enter random seed, not conditional
      column(3,
             h3("Random Seed"),
             helpText("a value to set seed before each run of clusterSingle
                      (so that all of the runs are run on the same subsample of the data). 
                      Note, if 'random.seed' is set, argument 'ncores' should NOT be passed via subsampleArgs; 
                      instead set the argument 'ncores' of clusterMany directly 
                      (which is preferred for improving speed anyway)."),
             numericInput(ns("random.seed"), label = NULL, value = 29, min = 1, step = 1)
             )
      
      ),
    
    fluidRow(
      #Many Isolated unconditional logical inputs
      
      column(3, 
             h3("Verbose?"),
             helpText("If TRUE it will print informative messages."),
             checkboxInput(ns("verbose"), label = NULL, value = FALSE)
      ),
      
      column(3, 
             h3("Run?"),
             helpText("If FALSE, doesn't run clustering, but just returns matrix of parameters that will be run,
                      for the purpose of inspection by user (with rownames equal to the names of the resulting 
                      column names of clMat object that would be returned if run=TRUE). Even if run=FALSE, however, 
                      the function will create the dimensionality reductions of the data indicated by the user input."),
             checkboxInput(ns("run"), label = NULL, value = TRUE)
             ),
      
      column(3, 
             h3("Erase Old?"),
             helpText("Only relevant if input x is of class ClusterExperiment. 
                      If TRUE, will erase existing workflow results (clusterMany as well as mergeClusters and combineMany).
                      If FALSE, existing workflow results will have '_i' added to the clusterTypes value, 
                      where i is one more than the largest such existing workflow clusterTypes."),
             checkboxInput(ns("eraseOld"), label = NULL, value = FALSE)
             )
      )
  )
}
    
specializedInputs <- function(id, label = " Specializedinputs") {
  
  ns <- NS(id)
  
  tagList(
    ####################
    #Lots of questions here:
    ####################
    
    #Choose clustering function, not conditional
    fluidRow(
      h2("Choose a Clustering Algorithm and input their arguments:"),
      column (12, 
              h3("Clustering algorithm:"),
              helpText("Choose what type of clustering method to use. "),
              checkboxGroupInput(ns("clusterAlg"), choices = c("Sequential Cluster", "Cluster Distance",
                                                               "Cluster Subsample"), 
                          label = NULL, selected = NULL)
      )
    ),
    
    
    #Input sequential clustering arguments, conditional upon sequential clustering choice
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Sequential Cluster'"),
                     h3("Sequential Clustering Arguments"),
                     fluidRow(
                       
                       #I have some further questions about this
                       #logical subsample, conditional on sequential
                       column(3,
                              h3("Subsample"),
                              helpText("whether to subsample via subsampleClustering to get the distance matrix at
                                       each iteration; otherwise the distance matrix is set by arguments to clusterD."),
                              checkboxInput(ns("subsampleSQC"), label = "Subsample?", value = FALSE)
                              ),
                       #need a better understanding of what top.can is & what default value is
                       #enter top.can, conditional on sequential
                       
                       column(6,
                              h3("top.can"),
                              helpText("only the top.can clusters from clusterD (ranked by 'orderBy' argument given to
                                       clusterD) will be compared pairwise for stability. Making this very big will 
                                       effectively remove this parameter and all pairwise comparisons of all clusters found 
                                       will be considered. This might result in smaller clusters being found. Current
                                       default is fairly large, so probably will have little effect."), 
                              numericInput(ns("top.canSQC"), min = 1, value = 666, label = NULL, step = 1)
                       )
                       
                       ),
                     
                     fluidRow(
                       #enter k0, conditional on sequential
                       
                       column(4,
                              h3("K0"),
                              helpText("The value of K at the first iteration of sequential algorithm" ),
                              numericInput(ns("k0SQC"), label = NULL, value = 10, step = 1)
                       ),
                       ## assuming value between 0.0 - 1.0
                       #enter Beta, conditional on sequential
                       
                       column(4,
                              h3("Beta"),
                              helpText("value between 0 and 1 to decide how stable clustership membership has to be before 
                                       'finding' and removing the cluster."),
                              numericInput(ns("betaSQC"), label = NULL, value = .5, min = 0, max = 1)                              ),
                       
                       # need default value
                       #Enter remain.n, conditional on sequential
                       
                       column(4,
                              h3("remain.n"),
                              helpText("when only this number of samples are left (i.e. not yet clustered) 
                                       then algorithm will stop."), 
                              numericInput(ns("remain.nSQC"), min = 1, value = 666, label = NULL, step = 1)
                              )
                       
                       ),
                     
                     fluidRow(
                       # need default value
                       #Enter kmin, conditional on sequential
                       
                       column(4,
                              h3("k.min"),
                              helpText("each iteration of sequential detection of clustering will decrease the beginning K 
                                       of subsampling, but not lower than k.min."), 
                              numericInput(ns("k.minSQC"), min = 1, value = 666, label = NULL, step = 1)
                              ),
                       # need default value
                       #enter k.max, conditional on sequential
                       
                       column(4,
                              h3("k.max"),
                              helpText("algorithm will stop if K in iteration is increased beyond this point."), 
                              numericInput(ns("k.maxSQC"), min = 1, value = 666, label = NULL, step = 1)
                       ),
                       
                       #logical verbose, conditional on sequential
                       
                       column(4,
                              h3("Verbose"),
                              helpText("whether the algorithm should print out information as to its progress."),
                              checkboxInput(ns("verboseSQC"), label = NULL, value = FALSE)
                       )
                     )
                     ),
    
    
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Cluster Distance'",
                                        "|| input['", ns("clusterAlg"), "'][1] == 'Cluster Distance'"),
                     h3("Cluster Distance Arguments"),
                     
                     fluidRow(
                       column(3, 
                              h3("Cluster Function"),
                              helpText("Unsure of how to allow user to input fuction")
                       ),
                       #Choose type of algorithm, conditional on Distance
                       
                       column(3, 
                              h3("Type of Algorithm"),
                              helpText("character value of either '01' or 'K' determining whether the function 
                                       given in clusterFunction should be called by clusterK or cluster01."),
                              selectInput(ns("typeAlgCD"), choices = c("NULL", "01", "K"), label = NULL)                              ), 
                       
                       column(3,
                              h3("Distance Function to be applied to matrix"),
                              helpText("Unsure of how to allow user to input fuction")
                       ),
                       column(3, 
                              h3("how to order the cluster"),
                              helpText("UNFINISHED - How to order the cluster (either by size or by maximum alpha value).")
                       )
                       ),
                     
                     fluidRow(
                       column(3,
                              h3("cluster Arguments"),
                              helpText("UNFINISHED - Arguments to be passed directly to the clusterFunction,
                                       beyond the required input.")
                              ),
                       
                       
                       #Can warnings be shown in Shiny? should they?
                       #Logical cheack arguments, conditional on Distance
                       
                       column(3,
                              h3("Check Cluster Arguments?"),
                              helpText("Logical as to whether should give warning if arguments given that don't match
                                       clustering choices given.Otherwise, inapplicable arguments will be ignored without warning."),
                              checkboxInput(ns("checkArgsCD"), label = "Check Arguments?", value = FALSE)                              ),
                       #Enter Alpha, conditional on DIstance
                       
                       
                       column(3,
                              h3("Alpha"),
                              helpText("a cutoff value of how much similarity needed for drawing blocks
                                       (lower values more strict)." ),
                              numericInput(ns("alphaCD"), label = "alpha", value = .01, step = .001)                              ),
                       #Enter Min size, conditional on Distance
                       
                       column(3,
                              h3("MinSize"),
                              helpText("the minimum number of samples in a cluster. Clusters found below this size will 
                                       be discarded and samples in the cluster will be given a cluster assignment of '-1'
                                       to indicate that they were not clustered."),
                              numericInput(ns("minSizeCD"), value = 2, label = NULL)                              )
                       )
                       ),
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Cluster Subsample'",
                                        "|| input['", ns("clusterAlg"), "'][1] == 'Cluster Subsample'",
                                        "|| input['", ns("clusterAlg"), "'][2] == 'Cluster Subsample'"),
                     h3("Cluster Subsample Arguments"),
                     fluidRow(
                       column(3, 
                              h3("Cluster Function"),
                              helpText("UNFINISHED - Unsure of how to allow user to input fuction. 
                                       Only allowing choice of 'pam' and 'kmeans'"),
                              selectInput(ns("clusterFunctionSC"), choices = c("kmeans", "pam"), label = NULL)
                              ),
                       column(3,
                              h3("cluster Arguments"),
                              helpText("UNFINISHED - Arguments to be passed directly to the clusterFunction,
                                       beyond the required input.")
                              ), 
                       #Choose type of classifying method, conditional on Subsample
                       
                       column(6,
                              h3("Classify Method"),
                              helpText("method for determining which samples should be used in the co-occurance matrix. 
                                       'All'= all samples, 'OutOfSample'= those not subsampled, and 'InSample'=those in the subsample.
                                       'All' and 'OutOfSample' require that you provide classifyFunction to define how to classify
                                       those samples not in the subsample into a cluster. If 'All' is chosen,
                                       all samples will be classified into clusters via the classifyFunctions, 
                                       not just those that are out-of-sample. Note if 'All' isn't chosen it is possible to get NAs in 
                                       resulting D matrix (particularly if not enough subsamples taken)."),
                              selectInput(ns("classifyMethodSC"), choices = c("All", "OutOfSample", "InSample"), label = NULL)                              )
                       ),
                     
                     fluidRow(
                       
                       column(4,
                              h3("Classify Function"),
                              helpText("UNFINISHED - A function which, given the output of clusterFunction and new data points, 
                                       will classify the new data points into a cluster.")
                              ),
                       #Need default value
                       #Enter number of resamples, conditional on Subsample
                       column(4,
                              h3("Number of resamples"),
                              helpText("The number of subsamples to draw." ),
                              numericInput(ns("resamp.numSC"), label = NULL, value = 10, step = 1)                       ),
                       ## assuming value between 0.0 - 1.0
                       #CEnter proportion of sampels, conditional on Subsample
                       
                       column(4,
                              h3("Proportion of Samples"),
                              helpText("The number of subsamples to draw. Please enter a number between 0 and 1"),
                              numericInput(ns("samp.pSC"), label = NULL, value = .5, min = 0, max = 1)                       )
                       
                    )
    )
    
)
}


#I may need to store vectors safely by assigning to variables and then inputting them


# Reactive function which builds the code being run by R:
makeCode <- function(input, output, session, stringsAsFactors) {
  clusterManyCode <- reactive({
    
    clusterManyCode <- paste(", dimReduce = c('", paste(input$dimReduce, collapse = "', '"), "')", sep = "")

    
    if("PCA" %in% input$dimReduce) {
      clusterManyCode <- paste(clusterManyCode, ", nPCADims = c(", input$nPCADims, ")", sep = "")
    }
    if("mad" %in% input$dimReduce || "cv" %in% input$dimReduce || "var" %in% input$dimReduce) {
      clusterManyCode <- paste(clusterManyCode, ", nVarDims = c(", input$nVarDims, ")", sep = "")
    }
    
    clusterManyCode <- paste(clusterManyCode, ", ks = c(", min(input$ks), ": ", max(input$ks), ")", 
                             ", clusterFunction = c('", paste(input$clusterFunction, collapse = "', '"), "')", sep = "")
    
    if("hierarchicalK" %in% input$clusterFunction || "pam" %in% input$clusterFunction) {
      clusterManyCode <- paste(clusterManyCode, 
                               ", findBestK = c(", paste(input$findBestK, collapse = ", "), ")",
                               ", removeSil = c(", paste(input$removeSil, collapse = ", "), ")", sep = "")
      if("TRUE" %in% input$removeSil) {
        clusterManyCode <- paste(clusterManyCode, ", silCutoff = c(", input$silCutoff, ")", sep = "")
      }
    }
    if("tight" %in% input$clusterFunction || "hierarchical01" %in% input$clusterFunction) {
      clusterManyCode <- paste(clusterManyCode, ", alphas = c(", input$alphas, ")", sep = "")
    }
    
    clusterManyCode <- paste(clusterManyCode, ", sequential = ", input$sequential, sep = "")
    
    if(input$sequential) {
      clusterManyCode <- paste(clusterManyCode, ", betas = ", input$betas, sep = "")
    }
    
    clusterManyCode <- paste(clusterManyCode, ", subsample = c(", paste(input$subsample, collapse = ", "), ")",
                             ", minSizes = c(", input$minSizes, ")",
                             ", ncores = ", input$ncores, ", random.seed = ", input$random.seed, 
                             ", verbose = ", input$verbose, ", run = ", input$run, 
                             ", eraseOld = ", input$eraseOld, sep = "")
    
    if ( "Sequential Cluster" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list( subsample = ", input$subsampleSQC, 
                               ", top.can = ", input$top.canSQC, ", k0 = ", input$k0SQC, 
                               ", beta = ", input$betaSQC, ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
                               ", kmax = ", input$k.maxSQC, ", verbose = ", input$verboseSQC, ")", sep = "")
    }
    
    if("Cluster Distance" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", clusterDArgs = list( typeAlg = ", input$typeAlgCD, 
                               ", checkArgs = ", input$checkArgsCD, 
                               ", alpha = ", input$alphaCD, ", minSize = ", input$minSizeCD,
                               ", clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
    }
    
    if ( "Cluster Subsample" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", subsampleArgs = list( classifyMethods = '", input$classifyMethodSC, 
                               "', resamp.numSC  = ", input$resamp.numSC, ", clusterFunction = '", input$clusterFunctionSC, 
                               "', samp.p = ", input$samp.pSC, ", clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
    }
    
    clusterManyCode <- paste(clusterManyCode, ")", sep = "")
    
    clusterManyCode
  })
  return(clusterManyCode)
}

clusterManyHelpText <- function() {
  paste("Welcome to the initial page in the cluster Experiment Shiny app...
        (Help Text continues)")
}


#Code that creates the actual global clusterExperiment Object
renderCE <- function(codeToBeEvaluated, dataframe) {
  #replacing the user's file string with the internal variable name
  innerCode <- sub(strsplit(codeToBeEvaluated, ",")[[1]][1],  "clusterMany(dataframe", expression, fixed = TRUE)
  
  
  eval(parse(text = innerCode))
  #cE <- paste(sum(dataframe[ ,1]), " + ", codeToBeEvaluated)

}




