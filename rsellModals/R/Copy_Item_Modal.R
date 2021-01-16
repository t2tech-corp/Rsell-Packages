#' Creates the Copy Inventory Item Modal
#'
#' This function Creates the Copy Inventory Item Modal.
#'
#' Requires: shiny
#'
#' @param  copy_search_list The copy search list for selection.
#' @return Modal Dialog in App
#' @export
#' @examples
#'

Copy_Item_Modal <- function(copy_search_list) {
    
    modal_title <- "Add New Item From Existing Item"
    
    showModal(modalDialog(
        
        title = modal_title,
                          
          fluidRow(
              
              column(width = 12, selectInput("c_inv_search", label = "Select Inventory Item To Copy", width = '100%', choices = c("", copy_search_list)))
              
          ),
          
          tags$hr(),
          
          tags$div(
              style = "text-align: center;",
              actionButton("copyconf", "Select", style = "color: white; background-color: #565455; border-color: #565455;", width = '25%'),
              tags$div(style = "display:inline-block; width: 40px", HTML("<br>")),
              actionButton("copycanc", "Cancel", style = "color: white; background-color: #d91e2f; border-color: #d91e2f;", width = '25%')
              
          ),
          
          footer = NULL,
          size = "m"
                          
    ))
    
}