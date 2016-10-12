#' @rdname InternalModules
#' @export
whatClusters <- function(id, label = "What Clusters Text") {
  ns <- NS(id)
  tagList(
    h4("Display what clusters:"),
    helpText("Would you like to display a current summary of the internal cluster experiment object?")
  )
}

#module to update which clusters -- to do
updateWhatClusters<- function(input, output, session) {
    #this is the wrong code...error in cut and paste.
    code <- reactive({
        code <- paste("")
        if(input$aProportion)
            code <- paste(code, ", proportion = ", input$proportion)
        if(input$aPropUnassigned)
            code <- paste(code, ", propUnassigned = ", input$propUnassigned)
        if(input$aMinSize)
            code <- paste(code, ", minSize = ", input$minSize)
        
        code <- paste(code, ")")
    })
    
    return(code)
}