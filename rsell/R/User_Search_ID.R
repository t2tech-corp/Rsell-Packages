#' Assigns the User ID Based on Role/Proxy Permissions
#'
#' This function Assigns the User ID Based on Role/Proxy Permissions.
#'
#' Requires: dplyr
#'
#' @return Inventory Table as a data frame.
#' @export
#'

User_Search_ID <- function() {
    
    if(session_user_record$role == "Primary") { db_user_id <- session_user_record$user_id  } else
    if(session_user_record$role == "Proxy")   { db_user_id <- session_user_record$proxy_id }
    
    return(db_user_id)
    
}