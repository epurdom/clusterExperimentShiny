shinyUI(fluidPage(
  titlePanel( "ClusterMany function in clusterExperiment"),
  
  fluidRow (
    column (3,
            h3("Data file:"),
            helpText("Upload the data on which to run the clustering. Can be: matrix (with genes in rows), 
                     a list of datasets overwhich the clusterings should be run, a SummarizedExperiment object, 
                     or a ClusterExperiment object."),
            fileInput("dataFile", label = NULL),
            textOutput("dataFileCode")
            
    ),
    column (3, 
            h3("Dimension Reduction:"),
            helpText("Choose what type of dimensionality reduction to perform before clustering. "),
            selectInput("dimReduce", choices = c("none","PCA", "var","cv", "mad"), 
            label = "Dimensionality Reduction Method:", selected = "none"),
            textOutput("dimReduceCode")
    ),
    column (3, 
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
    ),
    
    column (3, 
            h3("Transform Function:"),
            helpText("Help")
            # *Input(),
            # *Output("")
    )
    
  ),
  
  fluidRow(
    column(3, 
           h3("Are Data in Counts?"),
           helpText("Whether the data are in counts, in which case the default transFun argument is set as log2(x+1). 
                    This is simply a convenience to the user, "),
           checkboxInput("isCount", label = NULL, value = FALSE),
           textOutput("isCountCode")
    ),
    
    
    # I need some more technical knowhow to feel confident in my ks design
    
    column(3, 
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
           ),
    
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
    
    fluidRow(
      # I need some more technical knowhow to feel confident in my Sequential design
      column(3,
             h3("Sequential"),
             helpText("logical whether to use the sequential strategy"),
             checkboxInput("sequential", label = NULL, value = FALSE),
             textOutput("sequentialCode")
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
             h3("Subsample"),
             helpText("logical as to whether to subsample via subsampleClustering to get the distance matrix 
                      at each iteration; otherwise the distance function will be determined by argument 
                      distFunction passed in clusterDArgs."),
             checkboxInput("subsample", label = NULL, value = FALSE),
             textOutput("subsampleCode")
      )
    ),
    
    fluidRow(
      # This is buggy, I'm not sure why
      column(3,
             conditionalPanel(condition = "input.removeSil == TRUE",             
                              h3("Silhouette Cutoff, This is buggy"),
                              helpText("Requirement on minimum silhouette width to be included in cluster 
                                       (only if removeSil=TRUE)."),
                              numericInput("silCutoff", label = NULL, value = 0),
                              textOutput("silCutoffCode")
                              )
            ),
      
      column(3,
             h3("distFunction"),
             helpText("need clarity")
             )
    )
    
  )
))