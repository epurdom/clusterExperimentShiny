library(shiny)




shinyUI(fluidPage(
  titlePanel("ClusterMany function in clusterExperiment"),
  
  fluidRow (
    column (4,
            h3("Choose a Data File to Upload:"),
            helpText("Upload the data on which to run the clustering. Can be: matrix (with genes in rows), 
                     a list of datasets overwhich the clusterings should be run, a SummarizedExperiment object, 
                     or a ClusterExperiment object."),
            fileInput("dataFile", label = NULL,
                      accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain',
                        '.csv',
                        '.tsv'
                      )
            ),
            tags$hr(),
            checkboxInput('header', 'Header', TRUE),
            radioButtons('sep', 'Separator',
                         c(Comma=',',
                           Semicolon=';',
                           Tab='\t'),
                         ','),
            radioButtons('quote', 'Quote',
                         c(None='',
                           'Double Quote'='"',
                           'Single Quote'="'"),
                         '"'),
            tags$hr(),
            #may need some reworking
            tabPanel("dataFileCode", verbatimTextOutput("dataFileCode"))            
            
    ),
    column (4, 
            h3("Dimension Reduction:"),
            helpText("Choose what type of dimensionality reduction to perform before clustering. "),
            selectInput("dimReduce", choices = c("none","PCA", "var","cv", "mad"), 
            label = "Dimensionality Reduction Method:", selected = "none"),
            tabPanel("dimReduceCode", verbatimTextOutput("dimReduceCode"))            
    ),
    column (4, 
            conditionalPanel(condition = "input.dimReduce != 'PCA' && input.dimReduce != 'none'",             
                             h3("How many variables the dimensionality reduction should keep :"),
                             helpText("number of the most variable features to keep 
                                      (when 'var', 'cv', or 'mad' is identified in dimReduce). 
                                      If NA is included, then the full dataset will also be included."),
                             numericInput("nVarDims",
                                         label = "Number of dimensions to retain",
                                         value = 1),
                             tabPanel("nVarDimsCode", verbatimTextOutput("nVarDimsCode"))            
            ),
            
            conditionalPanel(condition = "input.dimReduce == 'PCA'",             
                             h3("Number of Principle Components to retain:"),
                             helpText("vector of the number of PCs to use (when 'PCA' is identified in dimReduce). 
                                      If NA is included, then the full dataset will also be included."),
                             numericInput("nPCADims",
                                          label = "Number of PCs to retain",
                                          value = 1),
                             tabPanel("nPCADimsCode", verbatimTextOutput("nPCADimsCode"))            
            )
    )
    
  ),
  
  fluidRow(
    # I need some more technical knowhow to feel confident in my ks design
    
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
           sliderInput("ks", label = "Range of K Values:", min = 1, max = 100, value = c(3,6),
                       step = 1, ticks = TRUE, round = TRUE),
           tabPanel("ksCode", verbatimTextOutput("ksCode"))            
    )
        ),
  
  fluidRow(
    
    column(3, 
            h3("Cluster Function:"),
            helpText("Function used for the clustering."),
            selectInput("clusterFunction", choices = c("tight", "hierarchical01","hierarchicalK", "pam"), 
                        label = "Function for Clustering:", selected = "tight"),
           tabPanel("clusterFunctionCode", verbatimTextOutput("clusterFunctionCode"))            
    ),
    
    column(3,
           conditionalPanel(condition = "input.clusterFunction == 'tight' || input.clusterFunction == 'hierarchical01'",             
                            h3("Alphas:"),
                            helpText("Values of alpha to be tried. 
                                     Determines tightness required in creating clusters from the dissimilarity matrix.
                                     In this box you are simply choosing the number of alphas to be tried. 
                                     Please enter numbers between 0.0 and 1.0, comma delimited." ),
                            textInput("alphas",
                                         label = "alpha vector",
                                         value = "0.25, 0.74"),
                            tabPanel("alphasCode", verbatimTextOutput("alphasCode"))            
           ),
           conditionalPanel(condition = "input.clusterFunction == 'hierarchicalK'",             
                            h3("Find Best K?"),
                            helpText("Whether should find best K based on average silhouette width
                                     (only used if clusterFunction of type 'K')"),
                            checkboxInput("findBestK", label = NULL, value = FALSE),
                            tabPanel("findBestKCode", verbatimTextOutput("findBestKCode"))            
           )
           ),
    
    column(3,
           conditionalPanel(condition = "input.clusterFunction == 'hierarchicalK'",             
                            h3("Remove Silhouette?"),
                            helpText("logical as to whether remove when silhouette < silCutoff
                                     (only used if clusterFunction of type 'K'')"),
                            checkboxInput("removeSil", label = NULL, value = FALSE),
                            tabPanel("removeSilCode", verbatimTextOutput("removeSilCode"))            
           )
                            ),
    column(3,
           conditionalPanel(condition = "input.removeSil",             
                            h3("Silhouette Cutoff"),
                            helpText("Requirement on minimum silhouette width to be included in cluster 
                                     (only if removeSil=TRUE)."),
                            numericInput("silCutoff", label = NULL, value = 0),
                            tabPanel("silCutoffCode", verbatimTextOutput("silCutoffCode"))            
           )
    )
  ),
  
    
    fluidRow(
      # I need some more technical knowhow to feel confident in my Sequential design
      column(3,
             h3("Sequential"),
             helpText("logical whether to use the sequential strategy"),
             checkboxInput("sequential", label = NULL, value = FALSE),
             tabPanel("sequentialCode", verbatimTextOutput("sequentialCode"))            
      ),
      
      
      column(6,
             conditionalPanel(condition = "input.sequential",             
                              h3("Betas:"),
                              helpText("values of beta to be tried in sequential steps. 
                                       Only used for sequential=TRUE. 
                                       Determines the similarity between two clusters required in order to deem 
                                       the cluster stable. Takes on values in [0,1]." ),
                              textInput("betas",
                                        label = "beta vector",
                                        value = NULL),
                              tabPanel("betasCode", verbatimTextOutput("betasCode"))            
             )
      )
      
    ),
    
    fluidRow(
      
      column(3,
             h3("distFunction"),
             helpText("need clarity")
             ),
      
      column(3,
             h3("Subsample"),
             helpText("logical as to whether to subsample via subsampleClustering to get the distance matrix 
                      at each iteration; otherwise the distance function will be determined by argument 
                      distFunction passed in clusterDArgs."),
             checkboxInput("subsample", label = NULL, value = FALSE),
             tabPanel("subsampleCode", verbatimTextOutput("subsampleCode"))            
      ),
      
      column (3, 
              h3("Transform Function:"),
              helpText("Help")
              # *Input(),
              # *Output("")
      ), 
      
      #This might need to be down with clusterD by line 27X
      
      column(3,
             h3("Minimum Cluster Sizes"),
             helpText("the minimimum size required for a cluster (in clusterD). 
                      Clusters smaller than this are not kept and samples are left unassigned.
                      Minimum cluster Size is 2."),
             numericInput("minSize", label = NULL, value = 5, min = 2, step = 1),
             tabPanel("minSizeCode", verbatimTextOutput("minSizeCode"))            
      )
    ),
  
    fluidRow(
      
      column(3,
             h3("Number of Cores"),
             helpText("The number of threads"),
             numericInput("ncores", label = NULL, value = 1, min = 1, step = 1),
             tabPanel("ncoresCode", verbatimTextOutput("ncoresCode"))            
      ),
      column(3,
             h3("Random Seed"),
             helpText("a value to set seed before each run of clusterSingle
                      (so that all of the runs are run on the same subsample of the data). 
                      Note, if 'random.seed' is set, argument 'ncores' should NOT be passed via subsampleArgs; 
                      instead set the argument 'ncores' of clusterMany directly 
                      (which is preferred for improving speed anyway)."),
             numericInput("random.seed", label = NULL, value = 29, min = 1, step = 1),
             tabPanel("random.seedCode", verbatimTextOutput("random.seedCode"))            
      )
      
    ),

    
    fluidRow(
      #I am throwing alot of isolated logical inputs here
      column(3, 
             h3("Are Data in Counts?"),
             helpText("Whether the data are in counts, in which case the default transFun argument is set as log2(x+1). 
                    This is simply a convenience to the user."),
             checkboxInput("isCount", label = NULL, value = FALSE),
             tabPanel("isCountCode", verbatimTextOutput("isCountCode"))            
      ),
      
      column(3, 
             h3("Verbose?"),
             helpText("If TRUE it will print informative messages."),
             checkboxInput("verbose", label = NULL, value = FALSE),
             tabPanel("verboseCode", verbatimTextOutput("verboseCode"))            
      ),
      
      column(3, 
             h3("Run?"),
             helpText("If FALSE, doesn't run clustering, but just returns matrix of parameters that will be run,
                      for the purpose of inspection by user (with rownames equal to the names of the resulting 
                      column names of clMat object that would be returned if run=TRUE). Even if run=FALSE, however, 
                      the function will create the dimensionality reductions of the data indicated by the user input."),
             checkboxInput("run", label = NULL, value = FALSE),
             tabPanel("runCode", verbatimTextOutput("runCode"))            
      ),
      
      column(3, 
             h3("Erase Old?"),
             helpText("Only relevant if input x is of class ClusterExperiment. 
                      If TRUE, will erase existing workflow results (clusterMany as well as mergeClusters and combineMany).
                      If FALSE, existing workflow results will have '_i' added to the clusterTypes value, 
                      where i is one more than the largest such existing workflow clusterTypes."),
             checkboxInput("eraseOld", label = NULL, value = FALSE),
             tabPanel("eraseOldCode", verbatimTextOutput("eraseOldCode"))            
      )
    ),
  
  
  ####################
  #Lots of questions here:
  ####################
  fluidRow(
    h2("Choose a Clustering Algorithm and input their arguments:"),
    column (12, 
            h3("Clustering algorithm:"),
            helpText("Choose what type of clustering method to use. "),
            selectInput("clusterAlg", choices = c("Cluster Distance","Cluster Subsample", 
                        "Sequential Cluster using Cluster Distance", "Sequential Cluster using Cluster Subsample"), 
                        label = NULL, selected = "Cluster Distance"),
            tabPanel("clusterAlgCode", verbatimTextOutput("clusterAlgCode"))            
    )
    
  ),
  conditionalPanel(condition = "input.clusterAlg == 'Sequential Cluster using Cluster Distance' || 
                                input.clusterAlg == 'Sequential Cluster using Cluster Subsample'",
                   h3("Sequential Clustering Arguments"),
                   fluidRow(
                     
                     #should I make this a select input kind of button?
                     column(3,
                            h3("Subsample"),
                            helpText("whether to subsample via subsampleClustering to get the distance matrix at
                                     each iteration; otherwise the distance matrix is set by arguments to clusterD."),
                            checkboxInput("subsampleSQC", label = "Subsample?", value = FALSE),
                            tabPanel("subsampleSQCCode", verbatimTextOutput("subsampleSQCCode"))            
                     ),
                     #need a better understanding of what top.can is & what default value is
                     column(6,
                            h3("top.can"),
                            helpText("only the top.can clusters from clusterD (ranked by 'orderBy' argument given to
                                     clusterD) will be compared pairwise for stability. Making this very big will 
                                     effectively remove this parameter and all pairwise comparisons of all clusters found 
                                     will be considered. This might result in smaller clusters being found. Current
                                     default is fairly large, so probably will have little effect."), 
                            numericInput("top.canSQC", min = 1, value = 666, label = NULL, step = 1),
                            tabPanel("top.canSQCCode", verbatimTextOutput("top.can.SQCCode"))            
                     )
                     
                   ),
                   
                   fluidRow(
                     
                     column(4,
                            h3("K0"),
                            helpText("The value of K at the first iteration of sequential algorithm" ),
                            numericInput("k0SQC", label = NULL, value = 10, step = 1),
                            tabPanel("k0SCQCode", verbatimTextOutput("k0SQCCode"))            
                     ),
                     ## assuming value between 0.0 - 1.0
                     column(4,
                            h3("Beta"),
                            helpText("value between 0 and 1 to decide how stable clustership membership has to be before 
                                     'finding' and removing the cluster."),
                            numericInput("betaSQC", label = NULL, value = .5, min = 0, max = 1),
                            tabPanel("betaSQCCode", verbatimTextOutput("betaSQCCode"))            
                     ),
                     
                     # need default value
                     column(4,
                            h3("remain.n"),
                            helpText("when only this number of samples are left (i.e. not yet clustered) 
                                     then algorithm will stop."), 
                            numericInput("remain.nSQC", min = 1, value = 666, label = NULL, step = 1),
                            tabPanel("remain.nCode", verbatimTextOutput("remain.nCode"))            
                     )
                     
                   ),
                   
                   fluidRow(
                     # need default value
                     column(4,
                            h3("k.min"),
                            helpText("each iteration of sequential detection of clustering will decrease the beginning K 
                                     of subsampling, but not lower than k.min."), 
                            numericInput("k.minSQC", min = 1, value = 666, label = NULL, step = 1),
                            tabPanel("k.minCode", verbatimTextOutput("k.minCode"))            
                     ),
                     # need default value
                     column(4,
                            h3("k.max"),
                            helpText("algorithm will stop if K in iteration is increased beyond this point."), 
                            numericInput("k.maxSQC", min = 1, value = 666, label = NULL, step = 1),
                            tabPanel("k.maxCode", verbatimTextOutput("k.maxCode"))            
                     ),
                     column(4,
                            h3("Verbose"),
                            helpText("whether the algorithm should print out information as to its progress."),
                            checkboxInput("verboseSQC", label = NULL, value = FALSE)),
                            tabPanel("verboseSQCCode", verbatimTextOutput("verboseSQCCode"))            
                   )
  ),
  
  conditionalPanel(condition = "input.clusterAlg == 'Cluster Distance' || 
                                input.clusterAlg == 'Sequential Cluster using Cluster Distance'",
                   h3("Cluster Distance Arguments"),
                   
                   fluidRow(
                     column(3, 
                            h3("Cluster Function"),
                            helpText("Unsure of how to allow user to input fuction")
                     ),
                     column(3, 
                            h3("Type of Algorithm"),
                            helpText("character value of either '01' or 'K' determining whether the function 
                                     given in clusterFunction should be called by clusterK or cluster01."),
                            selectInput("typeAlgCD", choices = c("NULL", "01", "K"), label = NULL),
                            tabPanel("typAlgCDCode", verbatimTextOutput("typeAlgCDCode"))            
                     ), 
                     column(3,
                            h3("Distance Function to be applied to matrix"),
                            helpText("Unsure of how to allow user to input fuction")
                     ),
                     column(3, 
                            h3("how to order the cluster"),
                            helpText("UNFINISHED - How to order the cluster (either by size or by maximum alpha value)."))
                    ),

                   fluidRow(
                     column(3,
                            h3("cluster Arguments"),
                            helpText("UNFINISHED - Arguments to be passed directly to the clusterFunction,
                                     beyond the required input.")
                            ),
                     

                     #Can warnings be shown in Shiny? should they?
                     column(3,
                            h3("Check Cluster Arguments?"),
                            helpText("Logical as to whether should give warning if arguments given that don't match
                            clustering choices given.Otherwise, inapplicable arguments will be ignored without warning."),
                            checkboxInput("checkArgsCD", label = "Check Arguments?", value = FALSE),
                            tabPanel("checkArgsCDCode", verbatimTextOutput("checkArgsCDCode"))            
                     ),

                     column(3,
                            h3("Alpha"),
                            helpText("a cutoff value of how much similarity needed for drawing blocks
                                     (lower values more strict)." ),
                            numericInput("alphaCD", label = "alpha", value = .01, step = .001),
                            tabPanel("alphaCDCode", verbatimTextOutput("alphaCDCode"))            
                     ),
                     column(3,
                            h3("MinSize"),
                            helpText("the minimum number of samples in a cluster. Clusters found below this size will 
                                     be discarded and samples in the cluster will be given a cluster assignment of '-1'
                                     to indicate that they were not clustered."),
                            numericInput("minSizeCD", value = 2, label = NULL),
                            tabPanel("minSizeCDCode", verbatimTextOutput("minSizeCDCode"))            
                            )
                   )
  ),
  
  conditionalPanel(condition = "input.clusterAlg == 'Cluster Subsample' || 
                                input.clusterAlg == 'Sequential Cluster using Cluster Subsample'",
                   h3("Cluster Subsample Arguments"),
                   fluidRow(
                     column(3, 
                            h3("Cluster Function"),
                            helpText("UNFINISHED - Unsure of how to allow user to input fuction")
                     ),
                     column(3,
                            h3("cluster Arguments"),
                            helpText("UNFINISHED - Arguments to be passed directly to the clusterFunction,
                                     beyond the required input.")
                            ), 
                     column(6,
                            h3("Classify Method"),
                            helpText("method for determining which samples should be used in the co-occurance matrix. 
                            'All'= all samples, 'OutOfSample'= those not subsampled, and 'InSample'=those in the subsample.
                            'All' and 'OutOfSample' require that you provide classifyFunction to define how to classify
                            those samples not in the subsample into a cluster. If 'All' is chosen,
                            all samples will be classified into clusters via the classifyFunctions, 
                            not just those that are out-of-sample. Note if 'All' isn't chosen it is possible to get NAs in 
                            resulting D matrix (particularly if not enough subsamples taken)."),
                            selectInput("classifyMethodSC", choices = c("All", "OutOfSample", "InSample"), label = NULL),
                            tabPanel("classifyMethodsSCCode", verbatimTextOutput("classifyMethodsSCCode"))            
                     )
                   ),
                   
                   fluidRow(
                     
                     column(4,
                            h3("Classify Function"),
                            helpText("UNFINISHED - A function which, given the output of clusterFunction and new data points, 
                                     will classify the new data points into a cluster.")
                     ),
                     #Need default value
                     column(4,
                            h3("Number of resamples"),
                            helpText("The number of subsamples to draw." ),
                            numericInput("resamp.numSC", label = NULL, value = 10, step = 1),
                            tabPanel("resamp.numSCCode", verbatimTextOutput("resamp.numSCCode"))            
                     ),
                     ## assuming value between 0.0 - 1.0
                     column(4,
                            h3("Proportion of Samples"),
                            helpText("The number of subsamples to draw. Please enter a number between 0 and 1"),
                            numericInput("samp.pSC", label = NULL, value = .5, min = 0, max = 1),
                            tabPanel("samp.pSCCode", verbatimTextOutput("samp.pSCCode"))            
                     )
                     
                   ),
                   fluidRow(
                     #I am just winging this so I can run the code
                     column(5,
                            h3("Arguments to chosen Cluster Function that Subsample is calling"),
                            helpText("manual input for now"),
                            textInput("clusterArgsSC", label = NULL, value = "Enter text..."),
                            tabPanel("clusterArgsSCCode", verbatimTextOutput("clusterArgsSCCode"))            
                            
                     )
                   )
                   ),
        
  fluidRow(
    column(12,
           h3("Code to be run in R:"),
           #tags$style(type='text/css', '#myoutput2 {background-color: rgba(0,0,255,0.10); color: blue;}'),
           
           tabPanel("ClusterManyCode", verbatimTextOutput("ClusterManyCode"))            
           )
  )
  
  
    
  )
)