#' Retrieve User Record from User Table
#'
#' This function Fetches the User Information for Logon and Authorization.
#'
#' Requires: RPostgres
#'
#' @param t_user The user ID to retrieve the record for.
#' @return user_record The user record in the table.
#' @export
#' @examples
#'

Get_User_Record <- function(t_user) {

    #### Refresh DB Connector if Needed ####

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ###

    my_query <- "SELECT * FROM user_table WHERE user_id = 'USERID'"

    my_query <- gsub("USERID", t_user, my_query)

    my_query <- RPostgres::dbSendQuery(db_con, my_query)

    user_record <- RPostgres::dbFetch(my_query)

    RPostgres::dbClearResult(my_query)

    ###

    return(user_record)

}
