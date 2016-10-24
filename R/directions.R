#################
##Description####
#This file, contains the directions text. Saved in one place to help make uniform formating choices, etc.
#################

#' @name directionsModules
#' @export
RSECHelpText<-function(){ "This is RSEC... more to come"}


#' @rdname directionsModules
#' @export
clusterManyHelpText <- function() {
    tagList(
        h4("Directions:"),
        p("The user should first set the the core imputs. After this, the user can choose to navigate to other tabs to find different options to vary. Different options will appear depending on the core inputs you choose, so choose these first."),
        p("For all these inputs choosing multiple values means that clusterings with all these values will be tried in combination with all of the other values also already chosen (except some global, esoteric ones under 'Specialized options'). If you do not choose the option, the (single) default will be run. "),
        p("No clusterings will be calculated until you press 'Run This Code' to the right. Under the 'Run This Code' button , you can see how many clusterings will be run based on the options you have chosen so far. ")
    )
}

#' @rdname directionsModules
#' @export
combineManyHelpText <- function() {
    tagList(
        h3("Directions"),
        p("combineMany will combine the clusters found by clusterMany into a single clustering. This will be done by comparing how frequently the samples co-cluster, across the clusterings found in clusterMany.")
    )
}

#help text
#' @rdname directionsModules
#' @export
makeDendrogramHelpText <- function() {
     tagList(
        h3("Directions"),
        p("makeDendrogram will create a hiearchical clustering of the clusters found by combineMany, based on performing hiearchical clustering on the mediod of the cluster.")
    )
}

#helptext
#' @rdname directionsModules
#' @export
mergeClustersHelpText <- function() {
    tagList(
        h3("Directions"),
        p("mergeClusters will merge sister clusters on the dendrogram based on the percentage of differentially expressed genes found between the two clusters. It will continue up the tree, merging resulting groups, until reaching two clusterings that have more that the desired cutoff of DE genes.")
    )
}

#helptext
#' @rdname directionsModules
#' @export
plotClustersHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Specialized Inputs for plotClusters()"),
        helpText("helptext here")
    )
}

#' @rdname InternalModules
#' @export
plotCoClusteringHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Specialized Inputs for plotCoClustering()"),
        helpText("helptext here")
    )
}

#help text
#' @rdname InternalModules
#' @export
plotDendrogramHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Specialized Inputs for plotDendrogram()"),
        helpText("helptext here")
    )
}

#' @rdname InternalModules
#' @export
plotHeatmapHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Specialized Inputs for plotHeatmap()"),
        helpText("helptext here")
    )
}
