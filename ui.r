library(shiny)




shinyUI(fluidPage(
  titlePanel("ClusterMany function in clusterExperiment"),
  
  fluidRow (
    column (4,
            h3("Data file:"),
            helpText("Upload the data on which to run the clustering. Can be: matrix (with genes in rows), 
                     a list of datasets overwhich the clusterings should be run, a SummarizedExperiment object, 
                     or a ClusterExperiment object."),
            fileInput("dataFile", label = NULL),
            textOutput("dataFileCode")
            
    ),
    column (4, 
            h3("Dimension Reduction:"),
            helpText("Choose what type of dimensionality reduction to perform before clustering. "),
            selectInput("dimReduce", choices = c("none","PCA", "var","cv", "mad"), 
            label = "Dimensionality Reduction Method:", selected = "none"),
            textOutput("dimReduceCode")
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
                             textOutput("nVarDimsCode")
            ),
            
            conditionalPanel(condition = "input.dimReduce == 'PCA'",             
                             h3("Number of Principle Components to retain:"),
                             helpText("vector of the number of PCs to use (when 'PCA' is identified in dimReduce). 
                                      If NA is included, then the full dataset will also be included."),
                             numericInput("nPCADims",
                                          label = "Number of PCs to retain",
                                          value = 1),
                             textOutput("nPCADimsCode")
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
                    ks also defines the kRange argument of clusterD unless kRange is specified by the user via the clusterDArgs; 
                    note this means that the default option of setting kRange that depends on the input k (see clusterD)
                    is not available in clusterMany."),
           sliderInput("ks", label = "Range of K Values:", min = 1, max = 100, value = c(3,6),
                       step = 1, ticks = TRUE, round = TRUE),
           textOutput("ksCode")
           )
        ),
  
  fluidRow(
    
    column(3, 
            h3("Cluster Function:"),
            helpText("Function used for the clustering."),
            selectInput("clusterFunction", choices = c("tight", "hierarchical01","hierarchicalK", "pam"), 
                        label = "Function for Clustering:", selected = "tight"),
            textOutput("clusterFunctionCode")
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
                                         value = NULL),
                            textOutput("alphasCode")
                            ),
           conditionalPanel(condition = "input.clusterFunction == 'hierarchicalK'",             
                            h3("Find Best K?"),
                            helpText("Whether should find best K based on average silhouette width
                                     (only used if clusterFunction of type 'K')"),
                            checkboxInput("findBestK", label = NULL, value = FALSE),
                            textOutput("findBestKCode")
                            )
           ),
    
    column(3,
           conditionalPanel(condition = "input.clusterFunction == 'hierarchicalK'",             
                            h3("Remove Silhouette?"),
                            helpText("logical as to whether remove when silhouette < silCutoff
                                     (only used if clusterFunction of type 'K'')"),
                            checkboxInput("removeSil", label = NULL, value = FALSE),
                            textOutput("removeSilCode")
                                     )
                            ),
    column(3,
           conditionalPanel(condition = "input.removeSil",             
                            h3("Silhouette Cutoff"),
                            helpText("Requirement on minimum silhouette width to be included in cluster 
                                     (only if removeSil=TRUE)."),
                            numericInput("silCutoff", label = NULL, value = 0),
                            textOutput("silCutoffCode")
                            )
    )
  ),
  
    
    fluidRow(
      # I need some more technical knowhow to feel confident in my Sequential design
      column(3,
             h3("Sequential"),
             helpText("logical whether to use the sequential strategy"),
             checkboxInput("sequential", label = NULL, value = FALSE),
             textOutput("sequentialCode")
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
                              textOutput("betasCode")
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
             textOutput("subsampleCode")
             ),
      
      column (3, 
              h3("Transform Function:"),
              helpText("Help")
              # *Input(),
              # *Output("")
      ), 
      
      column(3,
             h3("Minimum Cluster Sizes"),
             helpText("the minimimum size required for a cluster (in clusterD). 
                      Clusters smaller than this are not kept and samples are left unassigned.
                      Minimum cluster Size is 2."),
             numericInput("minSizes", label = NULL, value = 5, min = 2, step = 1),
             textOutput("minSizesCode")
             )
    ),
  
    fluidRow(
      
      column(3,
             h3("Number of Cores"),
             helpText("The number of threads"),
             numericInput("ncores", label = NULL, value = 1, min = 1, step = 1),
             textOutput("ncoresCode")
             ),
      column(3,
             h3("Random Seed"),
             helpText("a value to set seed before each run of clusterSingle
                      (so that all of the runs are run on the same subsample of the data). 
                      Note, if 'random.seed' is set, argument 'ncores' should NOT be passed via subsampleArgs; 
                      instead set the argument 'ncores' of clusterMany directly 
                      (which is preferred for improving speed anyway)."),
             numericInput("random.seed", label = NULL, value = 29, min = 1, step = 1),
             textOutput("random.seedCode")
      )
      
    ),
  
    fluidRow(
      h2("Arguments for chosen Clustering Algorithm:"),
      tabsetPanel(
        tabPanel("Cluster Distance Matrix Arguments",
                 column(3,
                        h3("Number of Cores"),
                        helpText("The number of threads"),
                        numericInput("ncoresCD", label = NULL, value = 1, min = 1, step = 1)
                        )
                 ),


        tabPanel("Cluster Subsample Arguments", helpText("Requisite subsampleClustering() args")),
        tabPanel("Sequential Clustering Arguments", helpText("Requisite seqCluster() args"))
      ),
      textOutput("ncoresCDCode")
      
    ),
    
    fluidRow(
      #I am throwing alot of isolated logical inputs here
      column(3, 
             h3("Are Data in Counts?"),
             helpText("Whether the data are in counts, in which case the default transFun argument is set as log2(x+1). 
                    This is simply a convenience to the user."),
             checkboxInput("isCount", label = NULL, value = FALSE),
             textOutput("isCountCode")
      ),
      
      column(3, 
             h3("Verbose?"),
             helpText("If TRUE it will print informative messages."),
             checkboxInput("verbose", label = NULL, value = FALSE),
             textOutput("verboseCode")
             ),
      
      column(3, 
             h3("Run?"),
             helpText("If FALSE, doesn't run clustering, but just returns matrix of parameters that will be run,
                      for the purpose of inspection by user (with rownames equal to the names of the resulting 
                      column names of clMat object that would be returned if run=TRUE). Even if run=FALSE, however, 
                      the function will create the dimensionality reductions of the data indicated by the user input."),
             checkboxInput("run", label = NULL, value = FALSE),
             textOutput("runCode")
      ),
      
      column(3, 
             h3("Erase Old?"),
             helpText("Only relevant if input x is of class ClusterExperiment. 
                      If TRUE, will erase existing workflow results (clusterMany as well as mergeClusters and combineMany).
                      If FALSE, existing workflow results will have '_i' added to the clusterTypes value, 
                      where i is one more than the largest such existing workflow clusterTypes."),
             checkboxInput("eraseOld", label = NULL, value = FALSE),
             textOutput("eraseOldCode")
             )
    )
    
  )
)