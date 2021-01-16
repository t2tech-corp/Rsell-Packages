#' LogoutUI Function
#'
#' This function processes the Log Out on button click.
#'
#' Requires: shinyjs
#'
#' @param id Namespace ID
#' @return Log In Panel
#' @export
#' @examples
#'

logoutUI <- function(id) {

    ns <- NS(id)

    hidden( actionButton(ns("button"), label = "Log Out", style = "color: white; background-color: #4c4c4f; border-color: #4c4c4f") )

}
