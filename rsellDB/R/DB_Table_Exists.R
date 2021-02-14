#' Check for Table in Database
#'
#' This function checks for the existence of a specific table in the database.
#'
#' Requires: RPostgres
#'
#' @param db_con The DB Connection parameters.
#' @param table_name The name of the DB Table to check.
#' @return table_exist Logical value.
#' @export
#'

DB_Table_Exists <- function(db_con, table_name) {

    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    table_exist <- RPostgres::dbExistsTable(conn = db_con, name = table_name)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Query Result --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(table_exist)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}
