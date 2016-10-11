#' @rdname InternalModules
#' @export
whatClusters <- function(id, label = "What Clusters Text") {
  ns <- NS(id)
  tagList(
    h4("Display what clusters:"),
    helpText("Would you like to display a current summary of the internal cluster experiment object?")
  )
}