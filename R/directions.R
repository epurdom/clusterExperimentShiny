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
    paste(
        # "Summary: The clusterMany function runs many clusterings at once. The user picks options that should be tried, and the clusterMany function will take all combinations of these options, and run the corresponding clustering for each. No clusterings will be calculated until you press 'Run This Code' to the right.",
        "Directions: The user should first set the the core imputs on the starting page. After this, the user can choose to navigate to other tabs to find different options to vary. For all these inputs choosing multiple values means that clusterings with all these values will be tried in combination with all of the other values also already chosen (except some global, esoteric ones under 'Specialized options'). If you do not choose the option, the (single) default will be run. No clusterings will be calculated until you press 'Run This Code' to the right. Under the 'Run This Code' button , you can see how many clusterings will be run based on the options you have chosen so far. "
    )
}

#' @rdname directionsModules
#' @export
combineManyHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Directions"),
        helpText("helptext here")
    )
}

#help text
#' @rdname directionsModules
#' @export
makeDendrogramHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Specialized Inputs for makeDendrogram()"),
        helpText("helptext here")
    )
}

#helptext
#' @rdname directionsModules
#' @export
mergeClustersHelpText <- function(id, label = "help title and text") {
    ns <- NS(id)
    tagList(
        h3("Directions"),
        helpText("helptext here")
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
