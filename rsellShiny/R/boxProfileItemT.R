#' Custom Boxprofile Item
#'
#' This function generates a custom Boxprofile Item. Built from shinydashboardPlus boxProfileItem.
#'
#' Requires:
#'
#' @param title Title
#' @param description Description
#' @return
#' @export
#' @examples
#'

boxProfileItemT <- function (title = NULL, description = NULL) {

    tags$li(class = "list-group-item", strong(title),
            a(class = "pull-right",  style = "color:black", description))

}
