#' Custom Boxprofile
#'
#' This function generates a custom Boxprofile. Built from shinydashboardPlus boxProfile.
#'
#' Requires:
#'
#' @param title Title
#' @param subtitle Subtitle
#' @return
#' @export
#' @examples
#'

boxProfileT <- function (..., title = NULL, subtitle = NULL) {

    tags$div(class = "box-body, box-profile",
             h3(class = "profile-username text-center", title),
             p(class = "text-muted text-center", subtitle),
             ...)

}
