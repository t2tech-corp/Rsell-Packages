#' Queries Database and Returns Specified Table Rows for User
#'
#' This function Queries Database and Returns Specified Table Rows for User
#'
#' Requires: RPostgres
#'
#' @param table_name The table name to perform query.
#' @param db_user_id The user ID for the table query.
#' @return query_table The returned table for user.
#' @export
#'

DB_Read_Table_User <- function(table_name, db_user_id = NULL) {

    ####  Check for Primary or Proxy User --------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(is.null(db_user_id)) { db_user_id <- User_Search_ID() }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Build Query ----------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    my_query <- "SELECT * FROM TABLENAME WHERE user_id = 'USERID'"

    my_query <- gsub("TABLENAME", table_name, my_query)
    my_query <- gsub("USERID",    db_user_id, my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    query_table <- RPostgres::dbGetQuery(db_con, my_query)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Query Result --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(query_table)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}
