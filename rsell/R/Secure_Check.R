#' Perform Security Check on Tab Access
#'
#' This function performs a security check for the tab to be accessed.
#'
#' Requires:
#'
#' @param input_tab Current Tab
#' @param tab_name Target Tab Name
#' @param user_auth Authorized User
#' @return auth_valid Logical return to allow/deny access.
#' @export
#'

Secure_Check <- function(input_tab, tab_name, user_auth) {

    auth_valid <- FALSE

    #

    if(is.null(input_tab))    { return(auth_valid) }
    if(input_tab != tab_name) { return(auth_valid) }

    if(user_auth) { auth_valid <- TRUE }

    #

    return(auth_valid)

}
