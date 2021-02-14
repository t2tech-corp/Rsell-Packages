#' List Fields in Database Table
#'
#' This function lists the fields in a database table.
#'
#' Requires: RPostgres
#'
#' @param db_con The DB Connection parameters.
#' @param table_name The name of the DB Table to check.
#' @return field_list Character array of field names.
#' @export
#'

DB_List_Fields <- function(db_con, table_name) {

    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    field_list <- RPostgres::dbListFields(conn = db_con, name = table_name)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Return Query Result --------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    return(field_list)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}
