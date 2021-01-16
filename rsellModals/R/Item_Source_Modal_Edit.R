#' Creates Item Source Modal for Edit Inventory Item
#'
#' This function Creates the Item Source Modal.
#'
#' Requires: shiny
#'
#' @return Modal Dialog in App
#' @export
#' @examples
#'

Item_Source_Modal_Edit <- function() {
    
    modal_title <- "Add New Item Source"
    
    showModal(modalDialog(
        
        title = tags$div(style = "text-align: center;", strong(modal_title)),
        
        textInput("new_item_source", label = "Enter New Source", value = ""),
        
        tags$hr(),
        
        tags$div(
            
            style = "text-align: center;",
            actionButton("eisubmit", "Add Source", style = "color: white; background-color: #565455; border-color: #565455;", width = '25%'),
            tags$div(style = "display:inline-block; width: 35px", HTML("<br>")),
            actionButton("eicancel", "Cancel",     style = "color: white; background-color: #d91e2f; border-color: #d91e2f;", width = '25%')
            
        ),
        
        footer = NULL,
        size =   "m"
        
    ))
    
}