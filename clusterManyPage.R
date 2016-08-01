#This file, clusterManyPage.R, contains the functions, inputs, and server-side computations of the clusterMany tab.

library(stringr)

#This userFile input is a very large function that recieves all of the inputs for clusterMany from the user

dimReduceInput <- function(id, label = "inputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(

      fluidRow(
        column(3, h3("Dimension Reduction:")),
        column(3, checkboxGroupInput(ns("dimReduce"), choices = c("none","PCA", "var","cv", "mad"), 
                                     label = "Dimensionality Reduction Method:", selected = "none")),
        column(6, h5("Description"),
               helpText("Choose what types of dimensionality reduction to perform before clustering. "))
      ),
      conditionalPanel(condition = paste0("input['", ns("dimReduce"), "'][0] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][1] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][2] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][3] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][4] == 'PCA'"),
                        tags$hr(),
                        fluidRow( 
                              #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
                          column(3, h3("Number of Principle Components to retain:")),
                          column(3, textInput(ns("nPCADims"), label = "Number of PCs to retain", value = 1)),
                          column(6, h4("Description"), helpText("please enter a list (separated by commas) of the 
                                                                number of PCs to use(when 'PCA' is identified in 
                                                                dimReduce).If NA is included, then the full dataset 
                                                                will also be included.")
                          )
                
                        )
      ),
      #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
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
                        tags$hr(),
                        fluidRow(
                
                          column(3, h3("How many variables the dimensionality reduction should keep :")),
                          column(3, textInput(ns("nVarDims"), label = "Number of dimensions to retain", value = 1)),
                          column(6, h4("Description"), helpText("Please enter a list (separated by commas) of the number
                                                        of the most variable features to keep (when 'var', 'cv', or 'mad' 
                                                        is identified in dimReduce). If NA is included, then the full
                                                        dataset will also be included."))
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
      column(3, h3("Sequential")),
      column(3, checkboxGroupInput(ns("sequential"), label = NULL, choices = c("TRUE", "FALSE"), selected = FALSE)),
      column(6, h4("Description"), helpText("logical whether to use the sequential strategy"))
    ),
      
      #Enter Betas, conditional upon sequential == TRUE
      #Danger! 
    conditionalPanel(condition = paste0("input['", ns("sequential"), "'][0] == 'TRUE'",
                                        "|| input['", ns("sequential"), "'][1] == 'TRUE'"),
                     tags$hr(),
                     fluidRow(
                       column(3, h3("Betas:")),
                       column(3, textInput(ns("betas"),
                                           label = "beta vector",
                                           value = NULL)
                       ),
                       column(6, h4("Description"), helpText("values of beta to be tried in sequential steps. 
                                       Only used for sequential=TRUE. 
                                       Determines the similarity between two clusters required in order to deem 
                                       the cluster stable. Takes on values in [0,1]." )
                       )
      ),
      #Logical subsample, not conditional 
      tags$hr(),
      fluidRow(
             column(3, h3("Subsample")),
             column(3, checkboxGroupInput(ns("subsample"), label = NULL, choices = c("TRUE", "FALSE"), selected = "FALSE")),
             column(6, h3("Description"), helpText("logical as to whether to subsample via subsampleClustering to get 
                                                    the distance matrix at each iteration; otherwise the distance 
                                                    function will be determined by argument distFunction passed in 
                                                    clusterDArgs."))
       ),
      
      conditionalPanel(condition = paste0("input['", ns("subsample"), "'][0] == 'FALSE'",
                                          "|| input['", ns("subsample"), "'][1] == 'FALSE'"),
                       tags$hr(),
                       fluidRow( 
                               h3("Transform Function:"),
                               helpText("Help")
                       )
      )
      
    ),
    tags$hr(),
    fluidRow(
      #Enter number of cores, not conditional
      column(3, h3("Number of Cores")),
      column(3, numericInput(ns("ncores"), label = NULL, value = 1, min = 1, step = 1)),
      column(6, h4("Description"), helpText("The number of threads"))
      ),
      
      #enter random seed, not conditional
    tags$hr(),
    fluidRow(             
      column(3,h3("Random Seed")),
      column(3, numericInput(ns("random.seed"), label = NULL, value = 29, min = 1, step = 1)),
      column(6, h4("Description"), helpText("a value to set seed before each run of clusterSingle
                      (so that all of the runs are run on the same subsample of the data). 
                      Note, if 'random.seed' is set, argument 'ncores' should NOT be passed via subsampleArgs; 
                      instead set the argument 'ncores' of clusterMany directly 
                      (which is preferred for improving speed anyway)."))
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
    h2("Choose a Clustering Algorithm and input their arguments:"),
    fluidRow(
      column(3, h3("Clustering algorithm:")),
      column(3, checkboxGroupInput(ns("clusterAlg"), choices = c("Sequential Cluster", "Cluster Distance",
                                                                 "Cluster Subsample"), 
                                   label = NULL, selected = NULL)),
      column(6, h4("Description"), helpText("Choose what type of clustering method to use. "))
    ),
    
    
    #Input sequential clustering arguments, conditional upon sequential clustering choice
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Sequential Cluster'"),
                     tags$hr(),
                     h3("Sequential Clustering Arguments"),
                     fluidRow(
                       #need a better understanding of what top.can is & what default value is
                       #enter top.can, conditional on sequential
                       
                       column(3, h3("top.can")),
                       column(3,numericInput(ns("top.canSQC"), min = 1, value = 666, label = NULL, step = 1)),
                       column(6, h4("Description"),
                              helpText("only the top.can clusters from clusterD (ranked by 'orderBy' argument given to
                              clusterD) will be compared pairwise for stability. Making this very big will 
                              effectively remove this parameter and all pairwise comparisons of all clusters found 
                              will be considered. This might result in smaller clusters being found. Current
                              default is fairly large, so probably will have little effect.")
                        )
                     ),
                     tags$hr(),
                     fluidRow(
                       column(3, h3("remain.n")),
                       column(3, numericInput(ns("remain.nSQC"), min = 1, value = 666, label = NULL, step = 1)),
                       column(6, h4("Description"),
                              helpText("when only this number of samples are left (i.e. not yet clustered) 
                                       then algorithm will stop.")
                       )
                     ),
                     tags$hr(),
                     fluidRow(
                       # need default value
                       #Enter kmin, conditional on sequential
                       
                       column(3, h3("k.min")),
                       column(3, numericInput(ns("k.minSQC"), min = 1, value = 666, label = NULL, step = 1)),
                       column(6, h4("Description"), 
                              helpText("each iteration of sequential detection of clustering will decrease the beginning K 
                                       of subsampling, but not lower than k.min.")
                       )
                     ),
                       # need default value
                       #enter k.max, conditional on sequential
                     tags$hr(),
                     fluidRow(
                        column(3, h3("k.max")),
                        column(3, numericInput(ns("k.maxSQC"), min = 1, value = 666, label = NULL, step = 1)),
                        column(6, h4("Description"), 
                               helpText("algorithm will stop if K in iteration is increased beyond this point.")
                        )
                     )
    ),
    
    
    
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Cluster Distance'",
                                        "|| input['", ns("clusterAlg"), "'][1] == 'Cluster Distance'"),
                     h3("Cluster Distance Argument"),
                     
                     fluidRow(
                       column(3, h3("cluster Arguments")),
                       column(3, h4("Decription"), helpText("UNFINISHED - Arguments to be passed directly to the clusterFunction,
                                       beyond the required input.")
                       )
                     )
    ),
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Cluster Subsample'",
                                        "|| input['", ns("clusterAlg"), "'][1] == 'Cluster Subsample'",
                                        "|| input['", ns("clusterAlg"), "'][2] == 'Cluster Subsample'"),
                     h3("Cluster Subsample Arguments"),
                     fluidRow(
                       column(3, h3("Cluster Function")),
                       column(3, selectInput(ns("clusterFunctionSC"), choices = c("kmeans", "pam"), label = NULL)),
                       column(6, h4("Description"), helpText("UNFINISHED - Unsure of how to allow user to input fuction. 
                                       Only allowing choice of 'pam' and 'kmeans'")
                       )
                     ),
                     tags$hr(),
                     fluidRow(
                       column(3, h3("cluster Arguments")),
                       column(6, h4("Description"), helpText("UNFINISHED - Arguments to be passed directly 
                                                             to the clusterFunction, beyond the required input.")
                       )
                      ), 
                       #Choose type of classifying method, conditional on Subsample
                     tags$hr(),
                     fluidRow(
                       column(3, h3("Classify Method")),
                       column(3, selectInput(ns("classifyMethodSC"), choices = c("All", "OutOfSample", "InSample"), 
                                             label = NULL)                              
                       ),
                       column(6, helpText("WHAT SHOULD I DELETE SPECIFICALLY? method for determining which samples should
                                          be used in the co-occurance matrix. 'All'= all samples, 'OutOfSample'= those 
                                          not subsampled, and 'InSample'=those in the subsample. 'All' and 'OutOfSample'
                                          require that you provide classifyFunction to define how to classify
                                          those samples not in the subsample into a cluster. If 'All' is chosen,
                                          all samples will be classified into clusters via the classifyFunctions, 
                                          not just those that are out-of-sample. Note if 'All' isn't chosen it is possible to get NAs in 
                                          resulting D matrix (particularly if not enough subsamples taken).")
                       )
                     ),
                     tags$hr(),
                     fluidRow(
                       #Need default value
                       #Enter number of resamples, conditional on Subsample
                       column(3, h3("Number of resamples")),
                       column(3, numericInput(ns("resamp.numSC"), label = NULL, value = 10, step = 1)),
                       column(6, h3("Description"), helpText("The number of subsamples to draw." ))
                     ),
                       ## assuming value between 0.0 - 1.0
                       #CEnter proportion of sampels, conditional on Subsample
                     tags$hr(),
                     fluidRow(
                       column(3, h3("Proportion of Samples")),
                       column(3, numericInput(ns("samp.pSC"), label = NULL, value = .5, min = 0, max = 1)),                    
                       column(6, h4("Description"), helpText("The number of subsamples to draw. Please enter a number
                                                             between 0 and 1")
                       )

                    )
    )
    
)}


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
    
    clusterManyCode <- paste(clusterManyCode, ", sequential = c(", paste(input$sequential, collapse = ", "), ")", sep = "")
    
    if("TRUE" %in% input$sequential) {
      clusterManyCode <- paste(clusterManyCode, ", betas = c(", input$betas, ")", sep = "")
    }
    
    clusterManyCode <- paste(clusterManyCode, ", subsample = c(", paste(input$subsample, collapse = ", "), ")",
                             ", minSizes = c(", input$minSizes, ")",
                             ", ncores = ", input$ncores, ", random.seed = ", input$random.seed, sep = "")
    
    if ( "Sequential Cluster" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list(", " top.can = ", input$top.canSQC, 
                               ", remain.n = ", input$remain.nSQC, ", kmin = ", input$k.minSQC,
                               ", kmax = ", input$k.maxSQC, ")", sep = "")
    }
    
    if("Cluster Distance" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
    }
    
    if ( "Cluster Subsample" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", subsampleArgs = list( classifyMethods = '", input$classifyMethodSC, 
                               "', resamp.numSC  = ", input$resamp.numSC,"', samp.p = ", input$samp.pSC, 
                               ", clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
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
  innerCode <- sub(strsplit(codeToBeEvaluated, ",")[[1]][1],  "clusterMany(dataframe", codeToBeEvaluated, fixed = TRUE)
  
  eval(parse(text = innerCode))
  #cE <- paste(sum(dataframe[ ,1]), " + ", codeToBeEvaluated)

}




