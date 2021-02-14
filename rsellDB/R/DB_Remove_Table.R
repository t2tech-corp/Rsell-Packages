#' Removes a Database Table
#'
#' This function Removes a Database Table.
#'
#' Requires: RPostgres
#'
#' @param db_con The DB Connection parameters.
#' @param table_name The name of the DB Table to remove.
#' @return
#' @export
#'

DB_Remove_Table <- function(db_con, table_name) {

    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    RPostgres::dbRemoveTable(conn = db_con, name = table_name)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}
