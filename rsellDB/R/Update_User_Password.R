#' Update User Record with New Password
#'
#' This function Updates User Record with new password.
#'
#' Requires: RPostgres
#'
#' @param pwd_hash The hashed password for the user.
#' @return session_user_record Updated session record.
#' @export
#' @examples
#'

Update_User_Password <- function(pwd_hash) {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ####

    my_query <- "UPDATE user_table SET pwd_hash = 'PWDHASH' WHERE user_id = 'USERID'"

    my_query <- gsub("USERID",  session_user_record$user_id, my_query)
    my_query <- gsub("PWDHASH", pwd_hash,                    my_query)

    RPostgres::dbBegin(db_con)

    RPostgres::dbExecute(db_con, my_query)

    RPostgres::dbCommit(db_con)

    #### Update User Record In Memory ####

    session_user_record$pwd_hash <<- pwd_hash

}
