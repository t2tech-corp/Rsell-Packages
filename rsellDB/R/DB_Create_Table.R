#' Creates a Database Table
#'
#' This function Creates a Database Table.
#'
#' Requires: RPostgres
#'
#' @param db_con The DB Connection parameters.
#' @param db_create_name The name of the DB Table to create.
#' @param table_create The data frame to use as the source for the new Table.
#' @return
#' @export
#'

DB_Create_Table <- function(db_con, db_create_name, table_create) {

    ####  Refresh DB Connector if Needed ---------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    if(!RPostgres::dbIsValid(db_con)) { db_con <- DB_Connect(db_name, db_host, db_user, db_pass) }

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####


    ####  Execute Query --------------------------------------------------------------------------------------------------------- ####
    ##                                                                                                                              ##

    RPostgres::dbCreateTable(conn = db_con, name = db_create_name, fields = table_create_name)

    ##                                                                                                                              ##
    #### ------------------------------------------------------------------------------------------------------------------------ ####

}
