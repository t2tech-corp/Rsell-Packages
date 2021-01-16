#' Update User Record with Last Logon Timestamp
#'
#' This function Updates User Record with Last Logon Timestamp.
#'
#' Requires: RPostgres
#'
#' @param user_record The User Record to update.
#' @return session_user_record Updated session information.
#' @export
#' @examples
#'

Update_User_Last <- function(user_record) {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ####

    my_query <- "UPDATE user_table SET last_logon = 'LASTLOGON' WHERE user_id = 'USERID'"

    my_query <- gsub("USERID",    user_record$user_id,    my_query)
    my_query <- gsub("LASTLOGON", user_record$last_logon, my_query)

    RPostgres::dbBegin(db_con)

    RPostgres::dbExecute(db_con, my_query)

    RPostgres::dbCommit(db_con)

    #### Update User Record In Memory ####

    session_user_record$last_logon <<- user_record$last_logon

}
