#' Logout Function
#'
#' This function process the Logout from the application.
#'
#' Requires: shinyjs, sodium
#'
#' @param input Session input
#' @param output Session output
#' @param session Session info
#' @param active Active check
#' @return Updated Credentials
#' @export
#' @examples
#'

logout <- function(input, output, session, active) {

    observeEvent(active(), ignoreInit = TRUE, {

        if(RPostgres::dbIsValid(db_con)) { DB_Disconnect(db_con) }

        toggle(id = "button", anim = TRUE, time = 1, animType = "fade")

    })

    # return reactive logout button tracker

    reactive({input$button})

}
