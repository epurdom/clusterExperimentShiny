

#This file, clusterManyPage.R, contains the functions, inputs, and server-side computations of the clusterMany tab.

library(stringr)

#This userFile input is a very large function that recieves all of the inputs for clusterMany from the user

dimReduceInput <- function(id, label = "inputs") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(

    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aDimReduce"), value = FALSE, label = "Add Dimensionailty Reduction?")),
      conditionalPanel(condition = paste0("input['", ns("aDimReduce"), "']"),
         column(3, checkboxGroupInput(ns("dimReduce"), choices = c("none","PCA", "var","cv", "mad"), label = "dimReduce")),
         column(2, checkboxInput(ns("hDimReduce"), value = FALSE, label = "Help Text and Instructions")),
         conditionalPanel(condition = paste0("input['", ns("hDimReduce"), "']"),
             column(4, helpText("what type of dimensionality reduction to perform before clustering.")
             )
         )
      )
    ),
      conditionalPanel(condition = paste0("input['", ns("dimReduce"), "'][0] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][1] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][2] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][3] == 'PCA'",
                                          "|| input['", ns("dimReduce"), "'][4] == 'PCA'"),
          tags$hr(),
          fluidRow( 
              #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
              column(3, checkboxInput(ns("aNPCADims"), value = FALSE, label = "Add nPCADims?")),
              conditionalPanel(condition = paste0("input['", ns("aNPCADims"), "']"),
                    column(3, textInput(ns("nPCADims"), value = "3, 5, 7", label = "nPCADims")),
                    column(2, checkboxInput(ns("hNPCADims"), value = FALSE, label = "Help Text and Instructions")),
                    conditionalPanel(condition = paste0("input['", ns("hNPCADims"), "']"),
                        column(4, helpText("what type of dimensionality reduction to perform before 
                                           clustering.")
                        )
                    )
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
            #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
            column(3, checkboxInput(ns("aNVarDims"), value = FALSE, label = "Add nVarDims?")),
            conditionalPanel(condition = paste0("input['", ns("aNVarDims"), "']"),
                column(3, textInput(ns("nVarDims"), value = "300, 500, 700", label = "nPCADims")),
                column(2, checkboxInput(ns("hVarDims"), value = FALSE, label = "Help Text and Instructions")),
                conditionalPanel(condition = paste0("input['", ns("hVarDims"), "']"),
                    column(4, helpText("Please enter a list (separated by commas) of the number
                                       of the most variable features to keep (when 'var', 'cv', or 'mad' 
                                       is identified in dimReduce). If NA is included, then the full
                                       dataset will also be included.")
                    )
                )
            )
        )
    )
    
  )
}

clusterFunctionInputs <- function(id, label = "inputs") {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
      column(3, checkboxInput(ns("aClusterFunction"), value = FALSE, label = "Add Cluster Function?")),
      conditionalPanel(condition = paste0("input['", ns("aClusterFunction"), "']"),
          column(3, checkboxGroupInput(ns("clusterFunction"),  label = "Cluster Function",
                                       choices = c("tight", "hierarchical01","hierarchicalK", "pam"))
          ),
          column(2, checkboxInput(ns("hClusterFunction"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hClusterFunction"), "']"),
                column(4, helpText("function used for the clustering.")
                )
          )
      )
    ),
    
      #input alpha, conditional on clusterFunction = "tight" or clusterFunction = "hierarchical01"
      #Danger!
      #horrible syntax and overkill, but what to do with the poor design of Shiny for this circumstance?
    conditionalPanel(condition = paste0("input['", ns("clusterFunction"), "'][0] == 'tight'", 
                                        " || input['", ns("clusterFunction"), "'][0] == 'hierarchical01' ",
                                        " || input['", ns("clusterFunction"), "'][1] == 'tight' ",
                                        " || input['", ns("clusterFunction"), "'][1] == 'hierarchical01' ",
                                        " || input['", ns("clusterFunction"), "'][2] == 'tight' ",
                                        " || input['", ns("clusterFunction"), "'][2] == 'hierarchical01' ",
                                        " || input['", ns("clusterFunction"), "'][3] == 'tight' ",
                                        " || input['", ns("clusterFunction"), "'][3] == 'hierarchical01' "),
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aAlphas"), value = FALSE, label = "Add Alphas?")),
          conditionalPanel(condition = paste0("input['", ns("aAlphas"), "']"),
              column(3, textInput(ns("alphas"),  label = "Alphas", value = ".3, .5, .7")
              ),
              column(2, checkboxInput(ns("hAlphas"), value = FALSE, 
                                      label = "Help Text and Instructions")
              ),
              conditionalPanel(condition = paste0("input['", ns("hAlphas"), "']"),
                  column(4, helpText("Values of alpha to be tried.Determines tightness required in 
                                      creating clusters from the dissimilarity matrix. In this box you
                                      are simply choosing the number of alphas to be tried. 
                                      Please enter numbers between 0.0 and 1.0, comma delimited."))
              )
          )
      )
    ),
             #find best K logical, conditional on clusterFunction = "hierarchicalK"
    conditionalPanel(condition = paste0("input['", ns("clusterFunction"), "'][0] == 'hierarchicalK'",
                                        " || input['", ns("clusterFunction"), "'][1] == 'hierarchicalK'",
                                        " || input['", ns("clusterFunction"), "'][2] == 'hierarchicalK'",
                                        " || input['", ns("clusterFunction"), "'][3] == 'hierarchicalK'",
                                        " || input['", ns("clusterFunction"), "'][0] == 'pam'",
                                        " || input['", ns("clusterFunction"), "'][1] == 'pam'",
                                        " || input['", ns("clusterFunction"), "'][2] == 'pam'",                                              
                                        " || input['", ns("clusterFunction"), "'][3] == 'pam'"),
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aFindBestK"), value = FALSE, label = "Add findBestK?")),
          conditionalPanel(condition = paste0("input['", ns("aFindBestK"), "']"),
              column(3, checkboxGroupInput(ns("findBestK"), label = NULL, choices = c("TRUE", "FALSE")))
          ),
          column(2, checkboxInput(ns("hFindBestK"), value = FALSE, 
                                      label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hFindBestK"), "']"),
                column(4, helpText("Whether should find best K based on average silhouette width (only
                                    used if clusterFunction of type 'K')"))
          )
        ),
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aRemoveSil"), value = FALSE, label = "Add removeSil?")),
          conditionalPanel(condition = paste0("input['", ns("aRemoveSil"), "']"),
              column(3, checkboxGroupInput(ns("removeSil"), label = NULL, choices = c("TRUE", "FALSE")))
          ),
          column(2, checkboxInput(ns("hRemoveSil"), value = FALSE, 
              label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hRemoveSil"), "']"),
                           column(4, helpText("logical as to whether remove when silhouette < silCutoff
                                              (only used if clusterFunction of type 'K')"))
          )
        ),
        conditionalPanel(condition = paste0("input['", ns("removeSil"), "'][0] == 'TRUE'",
                                            " && input['", ns("aRemoveSil"), "']"),             
            tags$hr(),
            #Enter Sil cutoff, conditional upon removeSil == TRUE,  
            #which is conditional upon clusterFunction = "hierarchicalK"
            fluidRow(
              column(3, checkboxInput(ns("aSilCutoff"), value = FALSE, label = "Add silCutoff?")),
              conditionalPanel(condition = paste0("input['", ns("aSilCutoff"), "']"),
                  column(3, textInput(ns("silCutoff"), label = NULL, value = "3, 5, 7"))
              ),
              column(2, checkboxInput(ns("hSilCutoff"), value = FALSE, 
                                      label = "Help Text and Instructions")
              ),
              conditionalPanel(condition = paste0("input['", ns("hSilCutoff"), "']"),
                  column(4, helpText("Requirement on minimum silhouette width to be included in cluster 
                                       (only if removeSil=TRUE). sequence (separated by commas)")
                  )
              )
            )
        )
    ),
    
    tags$hr(),
    fluidRow(
      # I need some more technical knowhow to feel confident in my ks design
      # K values, not conditional
      column(3, checkboxInput(ns("aKs"), value = FALSE, label = "Add ks?")),
      conditionalPanel(condition = paste0("input['", ns("aKs"), "']"),
          column(3, textInput(ns("ks"), label = "Range of K Values:", value = "3, 5, 7"))
      ),
      column(2, checkboxInput(ns("hKs"), value = FALSE, 
                              label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hKs"), "']"),
            column(4, helpText("The argument 'ks' is interpreted differently for different 
                                choices of the other parameters. When/if sequential=TRUE, ks 
                                defines the argument k0 of seqCluster. Otherwise, 'ks' values 
                                are set in both subsampleArgs[['k']] and clusterDArgs[['k']] 
                                that are passed to clusterD and subsampleClustering. This passing
                                of these arguments via subsampleArgs[['k']] will only have an 
                                effect if 'subsample=TRUE'. Similarly, the passing of 
                                clusterDArgs[['k']] will only have an effect when the 
                                clusterFunction argument includes a clustering algorithm of type 
                                'K'. When/if 'findBestK=TRUE', ks also defines the kRange argument
                                of clusterD unless kRange is specified by the user via the
                                clusterDArgs; note this means that the default option of setting
                                kRange that depends on the input k (see clusterD) is not available
                                in clusterMany.")
                  )
             )
    ),
    tags$hr(),
    fluidRow(
      
      column(3,
             h3("distFunction"),
             helpText("need clarity")
      )
    ),
      
    tags$hr(),
    # #This might need to be down with clusterD by line 27X
    # #Enter Min clustr Sizes, not conditional
    fluidRow(
      column(3, checkboxInput(ns("aMinSizes"), value = FALSE, label = "Add minSizes?")),
      conditionalPanel(condition = paste0("input['", ns("aMinSizes"), "']"),
                       column(3, textInput(ns("minSizes"), label = NULL, value = "3, 5, 7"))
      ),
      column(2, checkboxInput(ns("hMinSizes"), value = FALSE, 
                              label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hMinSizes"), "']"),
          column(4, helpText("the minimimum size required for a cluster (in clusterD). Clusters smaller
                              than this are not kept and samples are left unassigned.
                              Minimum cluster Size is 2. Please enter numbers separeted by commas")
          )
      )
    )
  )
}

sSBInputs <- function(id, label = "SSB inputs") {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      column(3, checkboxInput(ns("aSequential"), value = FALSE, label = "Add sequential?")),
      conditionalPanel(condition = paste0("input['", ns("aSequential"), "']"),
          column(3, checkboxGroupInput(ns("sequential"), label = NULL, 
                                      choices = c("TRUE", "FALSE"), selected = FALSE)
          )
      ),
      column(2, checkboxInput(ns("hSequential"), value = FALSE, 
                              label = "Help Text and Instructions")
      ),
      conditionalPanel(condition = paste0("input['", ns("hSequential"), "']"),
          column(4, helpText("Whether to use the sequential strategy."))
      )
    ),
      
      #Enter Betas, conditional upon sequential == TRUE
      #Danger! 
    conditionalPanel(condition = paste0("input['", ns("sequential"), "'][0] == 'TRUE'",
                                        "|| input['", ns("sequential"), "'][1] == 'TRUE'"),
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aBetas"), value = FALSE, label = "Add betas?")),
          conditionalPanel(condition = paste0("input['", ns("aBetas"), "']"),
              column(3, textInput(ns("betas"), label = NULL, value = ".3, .5, .7"))
          ),
          column(2, checkboxInput(ns("hBetas"), value = FALSE, label = "Help Text and Instructions")),
          conditionalPanel(condition = paste0("input['", ns("hBetas"), "']"),
              column(4, helpText("values of beta to be tried in sequential steps. Only used for 
                                 sequential=TRUE. Determines the similarity between two clusters 
                                 required in order to deem the cluster stable. Takes on values 
                                 in [0,1]." )
              )
          )
      ),
      #Logical subsample, not conditional
      tags$hr(),
      fluidRow(
        column(3, checkboxInput(ns("aSubsample"), value = FALSE, label = "Add subsample?")),
        conditionalPanel(condition = paste0("input['", ns("aSubsample"), "']"),
            column(3, checkboxGroupInput(ns("subsample"), label = NULL, choices = c("TRUE", "FALSE"), 
                                         selected = "FALSE")
            )
        ),
        column(2, checkboxInput(ns("hSubsample"), value = FALSE, label = "Help Text and Instructions")),
        conditionalPanel(condition = paste0("input['", ns("hSubsample"), "']"),
            column(4, helpText("logical as to whether to subsample via subsampleClustering to get 
                               the distance matrix at each iteration; otherwise the distance 
                               function will be determined by argument distFunction passed in 
                               clusterDArgs." )
            )
        )
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
      column(3, checkboxInput(ns("aNcores"), value = FALSE, label = "Add ncores?")),
      conditionalPanel(condition = paste0("input['", ns("aNcores"), "']"),
          column(3, numericInput(ns("ncores"), label = NULL, value = 1, min = 1, step = 1))
      ),
      column(2, checkboxInput(ns("hNcores"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hNcores"), "']"),
                       column(4, helpText("The number of threads." ))
      )
    ),
      
      #enter random seed, not conditional
    tags$hr(),
    fluidRow(
      column(3, checkboxInput(ns("aRandom.seed"), value = FALSE, 
                              label = "Add random.seed for reproducability?")
      ),
      conditionalPanel(condition = paste0("input['", ns("aRandom.seed"), "']"),
          column(3, numericInput(ns("random.seed"), label = NULL, value = 29, min = 1, step = 1))
      ),
      column(2, checkboxInput(ns("hRandom.seed"), value = FALSE, label = "Help Text and Instructions")),
      conditionalPanel(condition = paste0("input['", ns("hRandom.seed"), "']"),
          column(4, helpText("a value to set seed before each run of clusterSingle
                             (so that all of the runs are run on the same subsample of the data). 
                             Note, if 'random.seed' is set, argument 'ncores' should NOT be passed via subsampleArgs; 
                             instead set the argument 'ncores' of clusterMany directly 
                             (which is preferred for improving speed anyway)." )
          )
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
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aRemain.n"), value = FALSE, label = "Add remain.n?")
          ),
          conditionalPanel(condition = paste0("input['", ns("aRemain.n"), "']"),
              column(3, numericInput(ns("remain.n"), label = NULL, value = 29, min = 1, step = 1))
          ),
          column(2, checkboxInput(ns("hRemain.n"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hRemain.n"), "']"),
              column(4, helpText("when only this number of samples are left (i.e. not yet clustered)
                                 then algorithm will stop." )
              )
          )
        ),
        
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aTop.can"), value = FALSE, label = "Add top.can?")
          ),
          conditionalPanel(condition = paste0("input['", ns("aTop.can"), "']"),
              column(3, numericInput(ns("top.can"), label = NULL, value = 100, min = 1, step = 1))
          ),
          column(2, checkboxInput(ns("hTop.can"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hTop.can"), "']"),
              column(4, helpText("only the top.can clusters from clusterD (ranked by 'orderBy' 
                                 argument given to clusterD) will be compared pairwise for stability. 
                                 Making this very big will effectively remove this parameter and all
                                 pairwise comparisons of all clusters found will be considered. This
                                 might result in smaller clusters being found. Current default is 
                                 fairly large, so probably will have little effect." )
              )
          )
        ),

        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aKmin"), value = FALSE, label = "Add kmin?")
          ),
          conditionalPanel(condition = paste0("input['", ns("aKmin"), "']"),
              column(3, numericInput(ns("kmin"), label = NULL, value = 100, min = 1, step = 1))
          ),
          column(2, checkboxInput(ns("hKmin"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hKmin"), "']"),
              column(4, helpText("each iteration of sequential detection of clustering will decrease 
                                 the beginning K of subsampling, but not lower than k.min." )
              )
          )
        ),
                       #enter k.max, conditional on sequential
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aKmax"), value = FALSE, label = "Add kmax?")
          ),
          conditionalPanel(condition = paste0("input['", ns("aKmax"), "']"),
              column(3, numericInput(ns("kmax"), label = NULL, value = 100, min = 1, step = 1))
          ),
          column(2, checkboxInput(ns("hKmax"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hKmax"), "']"),
              column(4, helpText("algorithm will stop if K in iteration is increased beyond this 
                                 point.")
              )
          )
        )
    ),
    
    
    
    conditionalPanel(condition = paste0("input['", ns("clusterAlg"), "'][0] == 'Cluster Distance'",
                                        "|| input['", ns("clusterAlg"), "'][1] == 'Cluster Distance'"),
                     h3("Cluster Distance Argument"),
                     
                     fluidRow(
                       column(3, h5("cluster Arguments")),
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
            column(3, checkboxInput(ns("aClusterFunctionSC"), value = FALSE, label = "Add clusterFunction?")),
            conditionalPanel(condition = paste0("input['", ns("aClusterFunctionSC"), "']"),
                column(3, selectInput(ns("clusterFunctionSC"), choices = c("kmeans", "pam"), label = NULL))
            ),
            column(2, checkboxInput(ns("hClusterFunctionSC"), value = FALSE, 
                                    label = "Help Text and Instructions")
            ),
            conditionalPanel(condition = paste0("input['", ns("hClusterFunctionSC"), "']"),
                column(4, helpText("UNFINISHED - Unsure of how to allow user to input fuction. 
                                       Only allowing choice of 'pam' and 'kmeans'")
                )
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
          column(3, checkboxInput(ns("aClassifyMethod"), value = FALSE, label = "Add classifyMethod?")),
          conditionalPanel(condition = paste0("input['", ns("aClassifyMethod"), "']"),
              column(3, selectInput(ns("classifyMethod"), 
                                    choices = c("All", "OutOfSample", "InSample"), label = NULL)
              )
          ),
          column(2, checkboxInput(ns("hClassifyMethod"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hClassifyMethod"), "']"),
              column(4, helpText("WHAT SHOULD I DELETE SPECIFICALLY? method for determining which samples 
                                 should be used in the co-occurance matrix. 'All'= all samples, 
                                 'OutOfSample'= those not subsampled, and 'InSample'=those in the 
                                 subsample. 'All' and 'OutOfSample' require that you provide 
                                 classifyFunction to define how to classify those samples not in the 
                                 subsample into a cluster. If 'All' is chosen, all samples will be
                                 classified into clusters via the classifyFunctions, not just those
                                 that are out-of-sample. Note if 'All' isn't chosen it is possible to 
                                 get NAs in resulting D matrix (particularly if not enough subsamples
                                 taken).")
              )
          )
        ),
        tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aResamp.num"), value = FALSE, label = "Add resamp.num?")),
          conditionalPanel(condition = paste0("input['", ns("aResamp.num"), "']"),
                column(3, numericInput(ns("resamp.num"), label = NULL, value = 10, step = 1))
          ),
          column(2, checkboxInput(ns("hResamp.num"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hResamp.num"), "']"),
                           column(4, helpText("The number of subsamples to draw.")
                           )
          )
        ),

                       ## assuming value between 0.0 - 1.0
                       #CEnter proportion of sampels, conditional on Subsample
                     tags$hr(),
        fluidRow(
          column(3, checkboxInput(ns("aSamp.p"), value = FALSE, label = "Add samp.p?")),
          conditionalPanel(condition = paste0("input['", ns("aSamp.p"), "']"),
              column(3, numericInput(ns("samp.p"), label = NULL, value = 10, step = 1))
          ),
          column(2, checkboxInput(ns("hSamp.p"), value = FALSE, 
                                  label = "Help Text and Instructions")
          ),
          conditionalPanel(condition = paste0("input['", ns("hSamp.p"), "']"),
              column(4, helpText("the proportion of samples to sample for each subsample. Please 
                                 enter a number between 0 and 1")
              )
          )
        )
    )
    
)}


#I may need to store vectors safely by assigning to variables and then inputting them


# Reactive function which builds the code being run by R:
makeCode <- function(input, output, session, stringsAsFactors) {
  clusterManyCode <- reactive({
    
    clusterManyCode <- paste("")
    

    if(input$aSequential) {
      clusterManyCode <- paste(clusterManyCode, 
                               ", sequential = c(", paste(input$sequential, collapse = ", "), ")",
                               sep = "")
    }
    
    if("TRUE" %in% input$sequential && input$aBetas) {
      clusterManyCode <- paste(clusterManyCode, ", betas = c(", input$betas, ")", sep = "")
    }
    
    if(input$aSubsample) {
      clusterManyCode <- paste(clusterManyCode, 
                               ", subsample = c(", paste(input$subsample, collapse = ", "), ")", 
                               sep = "")
    }
    
    if(input$aNcores) {
      clusterManyCode <- paste(clusterManyCode, ", ncores = ", input$ncores, sep = "")
    }
    
    if(input$aRandom.seed) {
      clusterManyCode <- paste(clusterManyCode, ", random.seed = ", input$random.seed, sep = "")
    }
    
    if(input$aDimReduce) {
      clusterManyCode <- paste(", dimReduce = c('", paste(input$dimReduce, collapse = "', '"), "')", sep = "")
  
      
      if("PCA" %in% input$dimReduce && input$aNPCADims) {
        clusterManyCode <- paste(clusterManyCode, ", nPCADims = c(", input$nPCADims, ")", sep = "")
      }
      if(("mad" %in% input$dimReduce || "cv" %in% input$dimReduce || "var" %in% input$dimReduce)
         && input$aNVarDims) {
        clusterManyCode <- paste(clusterManyCode, ", nVarDims = c(", input$nVarDims, ")", sep = "")
      }
    }
    
    if(input$aKs){
      clusterManyCode <- paste(clusterManyCode, ", ks = c(", input$ks, ")", sep = "") 
    }
    if(input$aClusterFunction) {
      clusterManyCode <- paste(clusterManyCode, ", clusterFunction = c('", paste(input$clusterFunction, collapse = "', '"), "')", sep = "")
    
      if("hierarchicalK" %in% input$clusterFunction || "pam" %in% input$clusterFunction) {
        if(input$aFindBestK) {
        clusterManyCode <- paste(clusterManyCode,
                                 ", findBestK = c(", paste(input$findBestK, collapse = ", "), ")", 
                                 sep = "")
        }
        if(input$aRemoveSil) {
        clusterManyCode <- paste(clusterManyCode,
                                 ", removeSil = c(", paste(input$removeSil, collapse = ", "), ")", 
                                 sep = "")
        
          if("TRUE" %in% input$removeSil && input$aSilCutoff) {
            clusterManyCode <- paste(clusterManyCode, ", silCutoff = c(", input$silCutoff, ")", sep = "")
          }
        }
      }
      if(("tight" %in% input$clusterFunction || "hierarchical01" %in% input$clusterFunction) 
         && input$aAlphas) {
        clusterManyCode <- paste(clusterManyCode, ", alphas = c(", input$alphas, ")", sep = "")
      }
    }
    
    if(input$aMinSizes) {
      clusterManyCode <- paste(clusterManyCode, ", minSizes = c(", input$minSizes, ")")
    }
    

    if ( "Sequential Cluster" %in% input$clusterAlg) {
      #Initializing a counterSC for elegance in seqArgs()
      counterSC <- 0
      clusterManyCode <- paste(clusterManyCode, ", seqArgs = list(", sep = "")
      if(input$aRemain.n) {
          clusterManyCode <- paste(clusterManyCode, " remain.n = ", input$remain.n, sep = "")
          counterSC <- counterSC + 1
      }
      
      if(input$aTop.can) {
        if(counterSC == 0) {
          clusterManyCode <- paste(clusterManyCode, " top.can = ", input$top.can, sep = "") 
          counterSC <- counterSC + 1
        } else {
          clusterManyCode <- paste(clusterManyCode, ", top.can = ", input$top.can, sep = "") 
        }
      }
      
      if(input$aKmin) {
        if(counterSC == 0) {
          clusterManyCode <- paste(clusterManyCode, " kmin = ", input$kmin, sep = "")
          counterSC <- counterSC + 1
        } else {
          clusterManyCode <- paste(clusterManyCode, ", kmin = ", input$kmin, sep = "")
        }
      }

      if(input$aKmax) {
        if(counterSC == 0) {
          clusterManyCode <- paste(clusterManyCode, " kmax = ", input$kmax, sep = "")
          counterSC <- counterSC + 1
        } else {
          clusterManyCode <- paste(clusterManyCode, ", kmax = ", input$kmax, sep = "")
        }
      }
      
      clusterManyCode <- paste(clusterManyCode, ")", sep = "")

    }
    
    if("Cluster Distance" %in% input$clusterAlg) {
      clusterManyCode <- paste(clusterManyCode, ", clusterArgs = list(", input$clusterArgsSC, ")", sep = "")
    }
    
    if ( "Cluster Subsample" %in% input$clusterAlg) {
      counterC.S. <- 0
      
      clusterManyCode <- paste(clusterManyCode, ", subsampleArgs = list(", sep = "")
      
      if(input$aClusterFunctionSC) {
        clusterManyCode <- paste(clusterManyCode, "clusterFunction = '", input$clusterFunctionSC, "'",
                                 sep = "")
        counterC.S. <- counterC.S. + 1
      }
      
      if(input$aClassifyMethod) {
        if(counterC.S. == 0) {
          clusterManyCode <- paste(clusterManyCode, " classifyMethods = '", input$classifyMethod, 
                                   sep = "")
          counterC.S. <- counterC.S. + 1
        } else {
          clusterManyCode <- paste(clusterManyCode, ", classifyMethods = '", input$classifyMethod, "'", 
                                   sep = "")
        }
      }
      
      if(input$aResamp.num) {
        if(counterC.S. == 0) {
          clusterManyCode <- paste(clusterManyCode, " resamp.num = ", input$resamp.num, sep = "")
          counterC.S. <- counterC.S. + 1
        } else {
          clusterManyCode <- paste(clusterManyCode, ", resamp.num = ", input$resamp.num, sep = "")
        }
      }
      
      if(input$aSamp.p) {
        if(counterC.S. == 0) {
          clusterManyCode <- paste(clusterManyCode, " samp.p = ", input$samp.p, sep = "")
          counterC.S. <- counterC.S. + 1
        } else {
          clusterManyCode <- paste(clusterManyCode, ", samp.p = ", input$samp.p, sep = "")
        }
      }
      
      clusterManyCode <- paste(clusterManyCode, ")", sep = "")
      
      # clusterManyCode <- paste(clusterManyCode,  
      #                          ", clusterArgs = list(", input$clusterArgsSC, "))", sep = "")
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

# #Clean this up, unneccesary 
# #Code that creates the actual global clusterExperiment Object
# renderCE <- function(codeToBeEvaluated, dataframe) {
#   
#   #replacing the user's file string with the internal variable name
#   innerCode <- sub(strsplit(codeToBeEvaluated, ",")[[1]][1],  "clusterMany(dataframe", codeToBeEvaluated, fixed = TRUE)
#   
#   eval(parse(text = innerCode))
#   #cE <- paste(sum(dataframe[ ,1]), " + ", codeToBeEvaluated)
# 
# }

